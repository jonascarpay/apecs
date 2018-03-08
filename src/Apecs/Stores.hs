{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Stores
  ( Map, Cache, Unique,
    Global,
    Cachable,
  ) where

import           Control.Monad.Reader
import qualified Data.IntMap.Strict          as M
import           Data.IORef
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import           Apecs.Types

-- | A map from Data.Intmap.Strict. O(log(n)) for most operations.
--   Yields safe runtime representations of type @Maybe c@.
newtype Map c = Map (IORef (M.IntMap c))
instance Store (Map c) where
  type Elem (Map c) = c
  initStore = Map <$> newIORef mempty
  explDestroy (Map ref) ety = modifyIORef' ref (M.delete ety)
  explMembers (Map ref)     = U.fromList . M.keys <$> readIORef ref
  explExists  (Map ref) ety = M.member ety <$> readIORef ref
  explReset   (Map ref)     = writeIORef ref mempty
  explGet (Map ref) ety = fromJust . M.lookup ety <$> readIORef ref
  explSet       (Map ref) ety x = modifyIORef' ref $ M.insert ety x
  {-# INLINE explGet #-}
  {-# INLINE explSet #-}
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}

-- | A Unique contains at most one component.
--   Writing to it overwrites both the previous component and its owner.
data Unique c = Unique (IORef Int) (IORef c)
instance Store (Unique c) where
  type Elem (Unique c) = c
  initStore = Unique <$> newIORef (-1) <*> newIORef undefined
  explDestroy (Unique eref _) ety = do e <- readIORef eref; when (e==ety) (writeIORef eref (-1))

  explMembers (Unique eref _) = f <$> readIORef eref
    where f (-1) = mempty
          f x    = U.singleton x
  explReset   (Unique eref _) = writeIORef eref (-1)
  explExists  (Unique eref _) ety = (==ety) <$> readIORef eref
  {-# INLINE explDestroy #-}
  {-# INLINE explMembers #-}
  {-# INLINE explExists #-}
  {-# INLINE explReset #-}

  explGet (Unique _ cref) _ = readIORef cref

  explSet       (Unique eref cref) ety x = writeIORef eref ety >> writeIORef cref x
  {-# INLINE explSet #-}

-- | Constant value. Not very practical, but fun to write.
--   Contains `mempty`
newtype Const c = Const c
instance Monoid c => Store (Const c) where
  type Elem (Const c) = c
  initStore = return$ Const mempty
  explDestroy _ _ = return ()
  explExists  _ _  = return False
  explMembers _ = return mempty
  explReset _ = return ()
  explGet (Const c) _ = return c
  explSet       _ _ _ = return ()

-- | Global value.
--   Initialized with 'mempty'
newtype Global c = Global (IORef c)
instance Monoid c => Store (Global c) where
  type Elem   (Global c) = c
  initStore = Global <$> newIORef mempty

  explDestroy _ _ = return ()
  explExists _ _ = return True
  explGet (Global ref) _   = readIORef ref
  explSet       (Global ref) _ c = writeIORef ref c
  explMembers = return mempty


-- | A cache around another store.
--   The wrapped store must produce safe representations using Maybe.
--   Note that iterating over a cache is linear in its size, so large, sparsely populated caches might actually decrease performance.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s

class Store s => Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

instance (KnownNat n, Cachable s) => Store (Cache n s) where
  type Elem (Cache n s) = Elem s
  initStore = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- initStore
    return (Cache n tags cache child)

  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `rem` n) (-1)
       else explDestroy s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
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

  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x
