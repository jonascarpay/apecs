{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
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
newtype Unique c = Unique (IORef (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit (Unique c) where
  explInit = Unique <$> newIORef Nothing

instance ExplGet (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readIORef ref) $ \case
    Nothing -> error "Reading empty Unique"
    Just (_, c)  -> c
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((==ety) . fst) <$> readIORef ref

instance ExplSet (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = writeIORef ref (Just (ety, c))

instance ExplDestroy (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = readIORef ref >>=
    mapM_ (flip when (writeIORef ref Nothing) . (==ety) . fst)

instance ExplMembers (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readIORef ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

-- | A Global contains exactly one component.
--   The initial value is 'mempty' from the component's 'Monoid' instance.
--   When operating on a global, any entity arguments are ignored.
--   For example, we can get a global component with @get 0@ or @get 1@ or even @get undefined@.
newtype Global c = Global (IORef c)
type instance Elem (Global c) = c
instance Monoid c => ExplInit (Global c) where
  {-# INLINE explInit #-}
  explInit = Global <$> newIORef mempty

instance ExplGet (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = readIORef ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance ExplSet (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = writeIORef ref c

-- | An empty type class indicating that the store behaves like a regular map, and can therefore safely be cached.
--   An example of a store that cannot be cached is 'Unique'.
class Cachable s
instance Cachable (Map s)
instance (KnownNat n, Cachable s) => Cachable (Cache n s)

-- | A cache around another store.
--   Caches store their members in a fixed-size vector, so operations run in O(1).
--   Caches can provide huge performance boosts, especially for large numbers of components.
--   The cache size is given as a type-level argument.
--
--   Note that iterating over a cache is linear in cache size, so sparsely populated caches might actually decrease performance.
--   In general, the exact size of the cache does not matter as long as it reasonably approximates the number of components present.
--
--   The cache uses entity (-1) to internally represent missing entities, so be wary when manually manipulating entities.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s

cacheMiss :: t
cacheMiss = error "Cache miss!"

type instance Elem (Cache n s) = Elem s

instance (ExplInit s, KnownNat n, Cachable s) => ExplInit (Cache n s) where
  {-# INLINE explInit #-}
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-1)
    cache <- VM.replicate n cacheMiss
    child <- explInit
    return (Cache n tags cache child)

instance ExplGet s => ExplGet (Cache n s) where
  {-# INLINE explGet #-}
  explGet (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    if tag == ety
       then VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explExists #-}
  explExists (Cache n tags _ s) ety = do
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety then return True else explExists s ety

instance ExplSet s => ExplSet (Cache n s) where
  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

instance ExplDestroy s => ExplDestroy (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then do
         UM.unsafeWrite tags  index (-1)
         VM.unsafeWrite cache index cacheMiss
       else explDestroy s ety

instance ExplMembers s => ExplMembers (Cache n s) where
  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored
