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

import           Apecs.Core

-- | A map based on @Data.Intmap.Strict@. O(log(n)) for most operations.
newtype Map c = Map (IORef (M.IntMap c))

type instance Elem (Map c) = c
instance ExplInit (Map c) where
  explInit = Map <$> newIORef mempty

instance ExplGet (Map c) where
  explExists (Map ref) ety = M.member ety <$> readIORef ref
  explGet    (Map ref) ety = fromJust . M.lookup ety <$> readIORef ref
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplSet (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x = modifyIORef' ref $ M.insert ety x

instance ExplDestroy (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety = modifyIORef' ref (M.delete ety)

instance ExplMembers (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = U.fromList . M.keys <$> readIORef ref

-- | A Unique contains zero or one component.
--   Writing to it overwrites both the previous component and its owner.
--   Its main purpose is to be a @Map@ optimized for when only ever one component inhabits it.
data Unique c = Unique (IORef Int) (IORef c)
type instance Elem (Unique c) = c
instance ExplInit (Unique c) where
  explInit = Unique <$> newIORef (-1) <*> newIORef (error "Uninitialized Unique value")

instance ExplGet (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique _ cref) _ = readIORef cref
  {-# INLINE explExists #-}
  explExists (Unique eref _) ety = (==ety) <$> readIORef eref

instance ExplSet (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique eref cref) ety x = writeIORef eref ety >> writeIORef cref x

instance ExplDestroy (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique eref _) ety = do e <- readIORef eref; when (e==ety) (writeIORef eref (-1))

instance ExplMembers (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique eref _) = f <$> readIORef eref
    where f (-1) = mempty
          f x    = U.singleton x

-- | A Global contains exactly one component.
--   Initialized with 'mempty'
--   The store will return true for every existence check, but only ever gives (-1) as its inhabitant.
--   The entity argument is ignored when setting/getting a global.
newtype Global c = Global (IORef c)
type instance Elem (Global c) = c
instance Monoid c => ExplInit (Global c) where
  explInit = Global <$> newIORef mempty

instance ExplGet (Global c) where
  explGet (Global ref) _ = readIORef ref
  explExists _ _ = return True

instance ExplSet (Global c) where
  explSet (Global ref) _ c = writeIORef ref c

-- | An empty type class indicating that the store behaves like a regular map, and can therefore safely be cached.
class Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

-- | A cache around another store.
--   Note that iterating over a cache is linear in cache size, so sparsely populated caches might actually decrease performance.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s
type instance Elem (Cache n s) = Elem s

instance (ExplInit s, KnownNat n, Cachable s) => ExplInit (Cache n s) where
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.new n
    child <- explInit
    return (Cache n tags cache child)

instance ExplGet s => ExplGet (Cache n s) where
  explGet (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGet s ety

  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety then return True else explExists s ety

instance ExplSet s => ExplSet (Cache n s) where
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

instance ExplDestroy s => ExplDestroy (Cache n s) where
  explDestroy (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then UM.unsafeWrite tags (ety `rem` n) (-1)
       else explDestroy s ety

instance ExplMembers s => ExplMembers (Cache n s) where
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored
