{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Provides 'ChunkStore', a paged store whose page directory is a persistent
'Data.IntMap.Strict.IntMap'. Entities are grouped into fixed-size pages of @n@
slots (rounded up to the next power of two). A page is only allocated when the
first component in its entity range is written, so sparse regions cost nothing.

Within a dense run of consecutive entity IDs all components sit in the same
page, giving cache-friendly access. Because the 'IntMap' is a persistent PATRICIA
trie, structural updates share unchanged subtrees.

O(log P) per get/set/exists/destroy, where P is the number of allocated pages.
O(E) for 'explMembers', where E is the total number of components.

Convenience aliases:

* 'ChunkStoreU' — boxed leaf arrays, works for any component type.
* 'ChunkStoreU' — unboxed leaf arrays, requires 'Data.Vector.Unboxed.Unbox'.
-}
module Apecs.Experimental.ChunkStore
  ( ChunkStore (..)
  , ChunkStoreB
  , ChunkStoreU
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (countTrailingZeros, shiftL, shiftR, (.&.))
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Prim (RealWorld)
import GHC.TypeLits (KnownNat, Nat, natVal)

import Apecs.Core

-- | A fixed-size page covering @pageSize@ consecutive entity IDs.
data Page v c = Page
  { pageData :: !(v RealWorld c)
  , pagePresent :: !(UM.IOVector Bool)
  }

{- | Paged store parameterised over page size @n@ and vector backend @v@.
The page directory is a persistent 'IM.IntMap'; leaf arrays are mutable.
-}
data ChunkStore (n :: Nat) v c = ChunkStore
  { csPages :: !(IORef (IM.IntMap (Page v c)))
  , csPageSize :: !Int
  , csPageBits :: !Int -- log2 of page size, used for shiftR
  , csPageMask :: !Int -- page size - 1, used for .&.
  }

-- | 'ChunkStore' backed by boxed storage. Works for any component type.
type ChunkStoreB n = ChunkStore n VM.MVector

-- | 'ChunkStore' backed by unboxed storage. Requires 'U.Unbox'.
type ChunkStoreU n = ChunkStore n UM.MVector

type instance Elem (ChunkStore n v c) = c

nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go x
      | x >= n = x
      | otherwise = go (x `shiftL` 1)

newPage :: (GMV.MVector v c) => Int -> IO (Page v c)
newPage pageSize = Page <$> GMV.unsafeNew pageSize <*> UM.replicate pageSize False

instance (MonadIO m, KnownNat n, GMV.MVector v c) => ExplInit m (ChunkStore n v c) where
  explInit = liftIO $ do
    let
      pageSize = nextPow2 . fromIntegral $ natVal (Proxy @n)
      pageBits = countTrailingZeros pageSize
      pageMask = pageSize - 1
    ChunkStore <$> newIORef IM.empty <*> pure pageSize <*> pure pageBits <*> pure pageMask

instance (MonadIO m, GMV.MVector v c) => ExplGet m (ChunkStore n v c) where
  {-# INLINE explGet #-}
  explGet (ChunkStore pagesRef _ pageBits pageMask) ety = liftIO $ do
    pages <- readIORef pagesRef
    GMV.unsafeRead (pageData (pages IM.! (ety `shiftR` pageBits))) (ety .&. pageMask)

  {-# INLINE explExists #-}
  explExists (ChunkStore pagesRef _ pageBits pageMask) ety = liftIO $ do
    pages <- readIORef pagesRef
    case IM.lookup (ety `shiftR` pageBits) pages of
      Nothing -> pure False
      Just page -> UM.unsafeRead (pagePresent page) (ety .&. pageMask)

instance (MonadIO m, GMV.MVector v c) => ExplSet m (ChunkStore n v c) where
  {-# INLINE explSet #-}
  explSet (ChunkStore pagesRef pageSize pageBits pageMask) ety x = liftIO $ do
    pages <- readIORef pagesRef
    let
      pIdx = ety `shiftR` pageBits
      pOff = ety .&. pageMask
    page <- case IM.lookup pIdx pages of
      Just p -> pure p
      Nothing -> do
        p <- newPage pageSize
        writeIORef pagesRef (IM.insert pIdx p pages)
        pure p
    GMV.unsafeWrite (pageData page) pOff x
    UM.unsafeWrite (pagePresent page) pOff True

instance (MonadIO m) => ExplDestroy m (ChunkStore n v c) where
  {-# INLINE explDestroy #-}
  explDestroy (ChunkStore pagesRef _ pageBits pageMask) ety = liftIO $ do
    pages <- readIORef pagesRef
    case IM.lookup (ety `shiftR` pageBits) pages of
      Nothing -> pure ()
      Just page -> UM.unsafeWrite (pagePresent page) (ety .&. pageMask) False

instance (MonadIO m) => ExplMembers m (ChunkStore n v c) where
  {-# INLINE explMembers #-}
  explMembers (ChunkStore pagesRef _ pageBits _) = liftIO $ do
    pages <- readIORef pagesRef
    chunks <- mapM (uncurry membersInPage) (IM.toAscList pages)
    pure $! U.concat chunks
    where
      membersInPage pIdx page = do
        frozen <- U.unsafeFreeze (pagePresent page)
        let base = pIdx `shiftL` pageBits
        pure $! U.map ((base +) . fst) . U.filter snd $ U.indexed frozen
