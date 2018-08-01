{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Apecs.Stores
  ( Map, Cache, Unique,
    Global,
    Cachable,
    SimpleGlobal,
  ) where

import           Control.Concurrent.STM      as S
import           Control.Concurrent.STM.TVar as S

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
newtype Map c = Map (TVar (M.IntMap (TVar c)))

type instance Elem (Map c) = c
instance ExplInit (Map c) where
  explInit = Map <$> newTVarIO mempty

instance ExplGet IO (Map c) where
  explExists (Map ref) ety = M.member ety <$> readTVarIO ref
  explGet    (Map ref) ety =
    readTVarIO ref >>= readTVarIO . fromJust . M.lookup ety
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplSet IO (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x = do
    m <- readTVarIO ref
    case M.lookup ety m of
      Nothing -> do
        rInsert <- newTVarIO x
        atomically . writeTVar ref $ M.insert ety rInsert m

instance ExplDestroy IO (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety = do
    m <- readTVarIO ref
    atomically . writeTVar ref $ M.delete ety m

instance ExplMembers IO (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = U.fromList . M.keys <$> readTVarIO ref

instance ExplGet STM (Map c) where
  explExists (Map ref) ety = M.member ety <$> readTVar ref
  explGet    (Map ref) ety =
    readTVar ref >>= readTVar . fromJust . M.lookup ety
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplSet STM (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x = do
    m <- readTVar ref
    case M.lookup ety m of
      Nothing -> do
        rInsert <- newTVar x
        writeTVar ref $ M.insert ety rInsert m

instance ExplDestroy STM (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety = do
    m <- readTVar ref
    writeTVar ref $ M.delete ety m

instance ExplMembers STM (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = U.fromList . M.keys <$> readTVar ref

-- | A Unique contains zero or one component.
--   Writing to it overwrites both the previous component and its owner.
--   Its main purpose is to be a @Map@ optimized for when only ever one component inhabits it.
newtype Unique c = Unique (TVar (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit (Unique c) where
  explInit = Unique <$> newTVarIO Nothing

instance ExplGet IO (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readTVarIO ref) $ \case
    Nothing -> error "Reading empty Unique"
    Just (_, c)  -> c
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((==ety) . fst) <$> readTVarIO ref

instance ExplSet IO (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = atomically $ writeTVar ref (Just (ety, c))

instance ExplDestroy IO (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = readTVarIO ref >>=
    mapM_ (flip when (atomically $ writeTVar ref Nothing) . (==ety) . fst)

instance ExplMembers IO (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readTVarIO ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

instance ExplGet STM (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readTVar ref) $ \case
    Nothing -> error "Reading empty Unique"
    Just (_, c)  -> c
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((==ety) . fst) <$> readTVar ref

instance ExplSet STM (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = writeTVar ref (Just (ety, c))

instance ExplDestroy STM (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = readTVar ref >>=
    mapM_ (flip when (writeTVar ref Nothing) . (==ety) . fst)

instance ExplMembers STM (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readTVar ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

-- | A Global contains exactly one component.
--   The initial value is 'mempty' from the component's 'Monoid' instance.
--   When operating on a global, any entity arguments are ignored.
--   For example, we can get a global component with @get 0@ or @get 1@ or even @get undefined@.
newtype SimpleGlobal c = SimpleGlobal (IORef c)
type instance Elem (SimpleGlobal c) = c
instance Monoid c => ExplInit (SimpleGlobal c) where
  {-# INLINE explInit #-}
  explInit = SimpleGlobal <$> newIORef mempty

instance ExplGet IO (SimpleGlobal c) where
  {-# INLINE explGet #-}
  explGet (SimpleGlobal ref) _ = readIORef ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance ExplSet IO (SimpleGlobal c) where
  {-# INLINE explSet #-}
  explSet (SimpleGlobal ref) _ c = writeIORef ref c

newtype Global c = Global (TVar c)
type instance Elem (Global c) = c
instance Monoid c => ExplInit (Global c) where
  {-# INLINE explInit #-}
  explInit = Global <$> newTVarIO mempty

instance ExplGet IO (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = readTVarIO ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance ExplSet IO (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = atomically $ writeTVar ref c

instance ExplGet STM (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = readTVar ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance ExplSet STM (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = writeTVar ref c

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

instance ExplGet IO s => ExplGet IO (Cache n s) where
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

instance ExplSet IO s => ExplSet IO (Cache n s) where
  {-# INLINE explSet #-}
  explSet (Cache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags index
    when (tag /= (-1) && tag /= ety) $ do
      cached <- VM.unsafeRead cache index
      explSet s tag cached
    UM.unsafeWrite tags  index ety
    VM.unsafeWrite cache index x

instance ExplDestroy IO s => ExplDestroy IO (Cache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (Cache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- UM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then do
         UM.unsafeWrite tags  index (-1)
         VM.unsafeWrite cache index cacheMiss
       else explDestroy s ety

instance ExplMembers IO s => ExplMembers IO (Cache n s) where
  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-1)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored
