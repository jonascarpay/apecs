{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Apecs.Stores
  ( Map, Set, Cache,
    Global, readGlobal, writeGlobal,
    IndexTable, ToIndex(..), ByIndex(..), ByComponent(..),
  ) where

import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as VM
import Control.Monad
import Control.Monad.IO.Class
import GHC.TypeLits
import Data.Proxy

import Apecs.Core hiding (mapM_, forM_)

newtype Map c = Map (IORef (M.IntMap c))
instance Initializable (Map c) where
  type InitArgs (Map c) = ()
  initStoreWith _ = Map <$> newIORef mempty
instance HasMembers (Map c) where
  explDestroy (Map ref) ety = modifyIORef' ref (M.delete ety)
  explMembers (Map ref)     = U.fromList . M.keys <$> readIORef ref
  explExists  (Map ref) ety = M.member ety <$> readIORef ref
  explReset   (Map ref)     = writeIORef ref mempty
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
instance Store (Map c) where
  type SafeRW (Map c) = Maybe c
  type Stores (Map c) = c
  explGetUnsafe (Map ref) ety = fromJust . M.lookup ety <$> readIORef ref
  explGet       (Map ref) ety = M.lookup ety <$> readIORef ref
  explSet       (Map ref) ety x = modifyIORef' ref $ M.insert ety x
  explSetMaybe  s ety Nothing = explDestroy s ety
  explSetMaybe  s ety (Just x) = explSet s ety x
  explModify    (Map ref) ety f = modifyIORef' ref $ M.adjust f ety
  explCmap      (Map ref) f = modifyIORef' ref $ M.map f
  explCmapM_    (Map ref) ma = liftIO (readIORef ref) >>= Prelude.mapM_ ma
  explCmapM     (Map ref) ma = liftIO (readIORef ref) >>= Prelude.mapM  ma . M.elems
  explCimapM_   (Map ref) ma = liftIO (readIORef ref) >>= Prelude.mapM_ ma . M.assocs
  explCimapM    (Map ref) ma = liftIO (readIORef ref) >>= Prelude.mapM  ma . M.assocs
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}
  {-# INLINE explCmapM_ #-}
  {-# INLINE explCmapM #-}
  {-# INLINE explCimapM_ #-}
  {-# INLINE explCimapM #-}

newtype Set c = Set (IORef S.IntSet)
instance Initializable (Set c) where
  type InitArgs (Set c) = ()
  initStoreWith _ = Set <$> newIORef mempty
instance HasMembers (Set c) where
  explDestroy (Set ref) ety = modifyIORef' ref (S.delete ety)
  explMembers (Set ref) = U.fromList . S.toList <$> readIORef ref
  explReset (Set ref) = writeIORef ref mempty
  explExists (Set ref) ety = S.member ety <$> readIORef ref
  explImapM_  (Set ref) ma = liftIO (readIORef ref) >>= Prelude.mapM_ ma . S.toList
  explImapM   (Set ref) ma = liftIO (readIORef ref) >>= Prelude.mapM  ma . S.toList
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}
  {-# INLINE explImapM_ #-}
  {-# INLINE explImapM #-}
instance Store (Set c) where
  type SafeRW (Set c) = Bool
  type Stores (Set c) = c
  explGetUnsafe = error "Unsafe access to Set"
  explGet (Set ref) ety = S.member ety <$> readIORef ref
  explSet (Set ref) ety _ = modifyIORef' ref $ S.insert ety
  explSetMaybe s ety False = explDestroy s ety
  explSetMaybe s ety True  = explSet s ety (undefined :: c)
  explCmap _ _ = return ()
  explModify _ _ _ = return ()
  explCmapM   = error "Iterating over set"
  explCmapM_  = error "Iterating over set"
  explCimapM  = error "Iterating over set"
  explCimapM_ = error "Iterating over set"
  {-# INLINE explGetUnsafe #-}
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explSetMaybe #-}
  {-# INLINE explCmap #-}
  {-# INLINE explModify #-}

newtype Const c = Const c
instance Initializable (Const c) where
  type InitArgs (Const c) = c
  initStoreWith c = return$ Const c

instance GlobalRW (Const c) c where
  explGlobalRead  (Const c) = return c
  explGlobalWrite  _ _ = return ()
  explGlobalModify _ _ = return ()
instance HasMembers (Const c) where
  explDestroy _ _ = return ()
  explExists  _ _  = return False
  explMembers _ = return mempty
  explReset _ = return ()
instance Store (Const c) where
  type SafeRW (Const c) = c
  type Stores (Const c) = c
  explGetUnsafe (Const c) _ = return c
  explGet       (Const c) _ = return c
  explSet       _ _ _ = return ()
  explSetMaybe  _ _ _ = return ()
  explModify    _ _ _ = return ()
  explCmap       _ _ = return ()

newtype Global c = Global (IORef c)
instance Initializable (Global c) where
  type InitArgs (Global c) = c
  initStoreWith c = Global <$> newIORef c

instance GlobalRW (Global c) c where
  explGlobalRead   (Global ref) = readIORef    ref
  explGlobalWrite  (Global ref) = writeIORef   ref
  explGlobalModify (Global ref) = modifyIORef' ref
  {-# INLINE explGlobalRead #-}
  {-# INLINE explGlobalWrite #-}
  {-# INLINE explGlobalModify #-}

data Cache (n :: Nat) s =
  Cache Int -- | Size
        (UM.IOVector Int) -- | Tags
        (VM.IOVector (Stores s)) -- | Members
        s -- | Writeback

instance (KnownNat n, Initializable s) => Initializable (Cache n s) where
  type InitArgs (Cache n s) = (InitArgs s)
  initStoreWith args = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStoreWith args
    return (Cache n tags cache child)

instance HasMembers s => HasMembers (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `mod` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

  {-# INLINE explReset #-}
  explReset (Cache n tags _ s) = do
    forM_ [0..n-1] $ \e -> UM.write tags e (-1)
    explReset s

  {-# INLINE explImapM_ #-}
  explImapM_ (Cache _ tags _ s) ma = do
    liftIO (U.freeze tags) >>= U.mapM_ ma . U.filter (/= (-1))
    explImapM_ s ma

  {-# INLINE explImapM #-}
  explImapM (Cache _ tags _ s) ma = do
    as1 <- liftIO (U.freeze tags) >>= Prelude.mapM ma . U.toList . U.filter (/= (-1))
    as2 <- explImapM s ma
    return (as1 ++ as2)

instance (SafeRW s ~ Maybe (Stores s), Store s) => Store (Cache n s) where
  type SafeRW (Cache n s) = SafeRW s
  type Stores (Cache n s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (Cache n tags cache s) ety = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGetUnsafe s ety

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then Just <$> VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

  {-# INLINE explSetMaybe #-}
  explSetMaybe c ety Nothing  = explDestroy c ety
  explSetMaybe c ety (Just x) = explSet c ety x

  {-# INLINE explCmap #-}
  explCmap (Cache n tags cache s) f = do
    forM_ [0..n-1] $ \e -> do
      tag <- UM.read tags e
      unless (tag == (-1)) (VM.modify cache f e)
    explCmap s f

  {-# INLINE explModify #-}
  explModify (Cache n tags cache s) ety f = do
    let index = ety `mod` n
    tag <- UM.read tags index
    if tag == ety
       then VM.modify cache f ety
       else explModify s ety f

  {-# INLINE explCmapM_ #-}
  explCmapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma r
    explCmapM_ s ma

  {-# INLINE explCimapM_ #-}
  explCimapM_ (Cache n tags cache s) ma = do
    forM_ [0..n-1] $ \e -> do
      tag <- liftIO$ UM.read tags e
      unless (tag == (-1)) $ do
        r <- liftIO$ VM.read cache e
        void$ ma (e, r)
    explCimapM_ s ma

data SetCache (n :: Nat) s =
  SetCache Int -- | Size
           (UM.IOVector Int) -- | Tags
           s -- | Writeback

instance (KnownNat n, Initializable s) => Initializable (SetCache n s) where
  type InitArgs (SetCache n s) = (InitArgs s)
  initStoreWith args = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    child <- initStoreWith args
    return (SetCache n tags child)

instance HasMembers s => HasMembers (SetCache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (SetCache n tags s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `mod` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (SetCache n tags s) ety = do
    tag <- UM.unsafeRead tags (ety `mod` n)
    if tag == ety then return True else explExists s ety

  {-# INLINE explMembers #-}
  explMembers (SetCache _ tags s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

  explReset (SetCache n tags s) = do
    forM_ [0..n-1] $ \e -> UM.write tags e (-1)
    explReset s

instance (SafeRW s ~ Bool, Store s) => Store (SetCache n s) where
  type SafeRW (SetCache n s) = SafeRW s
  type Stores (SetCache n s) = Stores s

  {-# INLINE explGetUnsafe #-}
  explGetUnsafe = error "Unsafe access to Set"

  {-# INLINE explGet #-}
  explGet (SetCache n tags s) ety = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then return True
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (SetCache n tags s) ety _ = do
    let index = ety `mod` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      explSet s tag undefined
    UM.unsafeWrite tags  index ety

  {-# INLINE explSetMaybe #-}
  explSetMaybe c ety False = explDestroy c ety
  explSetMaybe c ety True  = explSet c ety undefined

  {-# INLINE explCmap #-}
  explCmap _ _ = return ()

  {-# INLINE explModify #-}
  explModify _ _ _ = return ()

-- | A component that can be hashed to a table index.
--   minBound must hash to the lowest possible value, maxBound must hash to the highest.
--   For Enums, toIndex = fromEnum
class Bounded a => ToIndex a where
  toIndex :: a -> Int
newtype ByIndex a     = ByIndex Int
newtype ByComponent c = ByComponent c
data IndexTable s = IndexTable
  { table :: VM.IOVector S.IntSet
  , wrapped :: s
  }

instance (ToIndex (Stores s), Initializable s) => Initializable (IndexTable s) where
  type InitArgs (IndexTable s) = InitArgs s
  initStoreWith args = do
    let lo = toIndex (minBound :: Stores s)
        hi = toIndex (maxBound :: Stores s)
        size = hi - lo + 1
    s <- initStoreWith args
    tab <- VM.replicate size mempty
    return (IndexTable tab s)

instance (SafeRW s ~ Maybe (Stores s), ToIndex (Stores s), Store s) => HasMembers (IndexTable s) where
  {-# INLINE explDestroy #-}
  explDestroy (IndexTable tab s) ety = do
    mc <- explGet s ety
    case mc of
      Just c -> do
        VM.modify tab (S.delete ety) (toIndex c)
        explDestroy s ety
      _ -> return ()

  {-# INLINE explExists #-}
  explExists  (IndexTable _ s) ety = explExists  s ety
  {-# INLINE explMembers #-}
  explMembers (IndexTable _ s) = explMembers s

  explReset (IndexTable tab s) = do
    forM_ [0 .. VM.length tab-1] $ \e -> VM.write tab e mempty
    explReset s

  {-# INLINE explImapM_ #-}
  explImapM_ (IndexTable _ s) = explImapM_ s

  {-# INLINE explImapM #-}
  explImapM (IndexTable _ s) = explImapM s

instance (SafeRW s ~ Maybe (Stores s), ToIndex (Stores s), Store s) => Store (IndexTable s) where
  type SafeRW (IndexTable s) = SafeRW s
  type Stores (IndexTable s) = Stores s
  {-# INLINE explGetUnsafe #-}
  explGetUnsafe (IndexTable _ s) ety = explGetUnsafe s ety
  {-# INLINE explGet #-}
  explGet (IndexTable _ s) ety = explGet s ety
  {-# INLINE explSet #-}
  explSet (IndexTable tab s) ety x = do
    let indexNew = toIndex x
    mc <- explGet s ety
    case mc of
      Nothing -> do VM.modify tab (S.insert ety) indexNew
      Just c  -> do let indexOld = toIndex c
                    unless (indexOld == indexNew) $ do
                      VM.modify tab (S.delete ety) indexOld
                      VM.modify tab (S.insert ety) indexNew
    explSet s ety x
  {-# INLINE explSetMaybe #-}
  explSetMaybe s ety Nothing = explDestroy s ety
  explSetMaybe s ety (Just x) = explSet s ety x
  {-# INLINE explModify #-}
  explModify (IndexTable tab s) ety f = do
    mc <- explGet s ety
    case mc of
      Nothing -> return ()
      Just c  -> do let indexOld = toIndex c
                        x = f c
                        indexNew = toIndex c
                    unless (indexOld == indexNew) $ do
                      VM.modify tab (S.delete ety) indexOld
                      VM.modify tab (S.insert ety) indexNew
                    explSet s ety x

  explCmapM_  (IndexTable _ s) = explCmapM_  s
  explCmapM   (IndexTable _ s) = explCmapM   s
  explCimapM_ (IndexTable _ s) = explCimapM_ s
  explCimapM  (IndexTable _ s) = explCimapM  s
  {-# INLINE explCmapM_ #-}
  {-# INLINE explCmapM #-}
  {-# INLINE explCimapM_ #-}
  {-# INLINE explCimapM #-}

instance (Stores s ~ c, ToIndex (Stores s)) => Query (ByComponent c) (IndexTable s) where
  {-# INLINE explSlice #-}
  explSlice (IndexTable tab _) (ByComponent c) = U.fromList . S.elems <$> VM.read tab (toIndex c)

instance (Stores s ~ c, ToIndex (Stores s)) => Query (ByIndex c) (IndexTable s) where
  {-# INLINE explSlice #-}
  explSlice (IndexTable tab _) (ByIndex ix) = U.fromList . S.elems <$> VM.read tab ix

