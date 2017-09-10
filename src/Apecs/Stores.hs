{-# LANGUAGE TypeApplications, DataKinds, BangPatterns, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Apecs.Stores
  ( Global, readGlobal, writeGlobal,
  ) where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import GHC.TypeLits
import Data.Proxy

import Apecs.Core

newtype Map c = Map (IORef (M.IntMap c))
instance Initializable (Map c) where
  type InitArgs (Map c) = ()
  initStore _ = Map <$> newIORef mempty
instance HasMembers (Map c) where
  explDestroy (Map !ref) !ety = modifyIORef' ref (M.delete ety)
  explMembers (Map !ref)      = U.fromList . M.keys <$> readIORef ref
  explExists  (Map !ref) !ety = M.member ety <$> readIORef ref
  explReset   (Map !ref)      = writeIORef ref mempty
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
instance Store (Map c) where
  type SafeRW (Map c) = Maybe c
  type Stores (Map c) = c
  explGetUnsafe (Map !ref) !ety = fromJust . M.lookup ety <$> readIORef ref
  explGet       (Map !ref) !ety = M.lookup ety <$> readIORef ref
  explSet       (Map !ref) !ety x = modifyIORef' ref $ M.insert ety x
  explSetMaybe  s ety Nothing = explDestroy s ety
  explSetMaybe  s ety (Just x) = explSet s ety x
  explMap       (Map !ref) f = modifyIORef' ref $ M.map f
  explModify    (Map !ref) ety f = modifyIORef' ref $ M.adjust f ety
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explMap #-}
  {-# INLINE explModify #-}

newtype Set c = Set (IORef S.IntSet)
instance Initializable (Set c) where
  type InitArgs (Set c) = ()
  initStore _ = Set <$> newIORef mempty
instance HasMembers (Set c) where
  explDestroy (Set ref) ety = modifyIORef' ref (S.delete ety)
  explMembers (Set ref) = U.fromList . S.toList <$> readIORef ref
  explReset (Set ref) = writeIORef ref mempty
  explExists (Set ref) ety = S.member ety <$> readIORef ref
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
instance Store (Set c) where
  type SafeRW (Set c) = Const Bool c
  type Stores (Set c) = c
  explGetUnsafe _ _ = return $ error "Unsafe access to Set"
  explGet (Set ref) ety = Const . S.member ety <$> readIORef ref
  explSet (Set ref) ety _ = modifyIORef' ref $ S.insert ety
  explSetMaybe s ety (Const False) = explDestroy s ety
  explSetMaybe s ety (Const True)  = explSet s ety (undefined :: c)
  explMap _ _ = return ()
  explModify _ _ _ = return ()
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explMap #-}
  {-# INLINE explModify #-}

newtype Global c = Global (IORef c)
instance Initializable (Global c) where
  type InitArgs (Global c) = c
  initStore c = Global <$> newIORef c

instance GlobalRW (Global c) c where
  explGlobalRead   (Global ref) = readIORef    ref
  explGlobalWrite  (Global ref) = writeIORef   ref
  explGlobalModify (Global ref) = modifyIORef' ref
  {-# INLINE explGlobalRead #-}
  {-# INLINE explGlobalWrite #-}
  {-# INLINE explGlobalModify #-}

data Cache (n :: Nat) s =
  Cache !Int -- | Size
        !(UM.IOVector Int) -- | Tags
        !(VM.IOVector (Stores s)) -- | Members
        !s -- | Writeback

instance (KnownNat n, Initializable s) => Initializable (Cache n s) where
  type InitArgs (Cache n s) = (InitArgs s)
  initStore args = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStore args
    return (Cache n tags cache child)

instance HasMembers s => HasMembers (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) !ety = do
    !tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `mod` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) !ety = do
    !tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

instance (SafeRW s ~ Maybe (Stores s), Store s) => Store (Cache n s) where
  type SafeRW (Cache n s) = SafeRW s
  type Stores (Cache n s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Cache n tags cache s) !ety = do
    let !index = ety `mod` n
    !tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGetUnsafe s ety

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) !ety = do
    let !index = ety `mod` n
    !tag <- UM.unsafeRead tags index
    if tag == ety
       then Just <$> VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) !ety x = do
    let !index = ety `mod` n
    !tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      !cached <- VM.unsafeRead cache index
      explSet s ety cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

  {-# INLINE explSetMaybe #-}
  explSetMaybe c ety Nothing = explDestroy c ety
  explSetMaybe c ety (Just x) = explSet c ety x

  {-# INLINE explMap #-}
  explMap (Cache n tags cache s) !f = forM_ [0..n-1] $ \e -> do
    !tag <- UM.read tags e
    unless (tag == (-1)) (VM.modify cache f e)
    explMap s f

  {-# INLINE explModify #-}
  explModify (Cache n tags cache s) !ety f = do
    let !index = ety `mod` n
    !tag <- UM.read tags index
    if tag == ety
       then VM.modify cache f ety
       else explModify s ety f
