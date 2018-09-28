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
    Register, regLookup
  ) where

import           Control.Monad.Reader
import qualified Data.IntMap.Strict          as M
import qualified Data.IntSet                 as S
import           Data.IORef
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import           Apecs.Core

-- | A map based on @Data.IntMap.Strict@. O(log(n)) for most operations.
newtype Map c = Map (IORef (M.IntMap c))

type instance Elem (Map c) = c
instance ExplInit IO (Map c) where
  explInit = Map <$> newIORef mempty

instance ExplGet IO (Map c) where
  explExists (Map ref) ety = M.member ety <$> readIORef ref
  explGet    (Map ref) ety =
    fromJust . M.lookup ety <$> readIORef ref
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance ExplSet IO (Map c) where
  {-# INLINE explSet #-}
  explSet (Map ref) ety x =
    modifyIORef' ref (M.insert ety x)

instance ExplDestroy IO (Map c) where
  {-# INLINE explDestroy #-}
  explDestroy (Map ref) ety =
    readIORef ref >>= writeIORef ref . M.delete ety

instance ExplMembers IO (Map c) where
  {-# INLINE explMembers #-}
  explMembers (Map ref) = U.fromList . M.keys <$> readIORef ref

-- | A Unique contains zero or one component.
--   Writing to it overwrites both the previous component and its owner.
--   Its main purpose is to be a @Map@ optimized for when only ever one component inhabits it.
newtype Unique c = Unique (IORef (Maybe (Int, c)))
type instance Elem (Unique c) = c
instance ExplInit IO (Unique c) where
  explInit = Unique <$> newIORef Nothing

instance ExplGet IO (Unique c) where
  {-# INLINE explGet #-}
  explGet (Unique ref) _ = flip fmap (readIORef ref) $ \case
    Nothing -> error "Reading empty Unique"
    Just (_, c)  -> c
  {-# INLINE explExists #-}
  explExists (Unique ref) ety = maybe False ((==ety) . fst) <$> readIORef ref

instance ExplSet IO (Unique c) where
  {-# INLINE explSet #-}
  explSet (Unique ref) ety c = writeIORef ref (Just (ety, c))

instance ExplDestroy IO (Unique c) where
  {-# INLINE explDestroy #-}
  explDestroy (Unique ref) ety = readIORef ref >>=
    mapM_ (flip when (writeIORef ref Nothing) . (==ety) . fst)

instance ExplMembers IO (Unique c) where
  {-# INLINE explMembers #-}
  explMembers (Unique ref) = flip fmap (readIORef ref) $ \case
    Nothing -> mempty
    Just (ety, _) -> U.singleton ety

-- | A Global contains exactly one component.
--   The initial value is 'mempty' from the component's 'Monoid' instance.
--
--   When operating on a global, any entity arguments are ignored.
--   For example, we can get a global component with @get 0@ or @get 1@ or even @get undefined@.
newtype Global c = Global (IORef c)
type instance Elem (Global c) = c
instance Monoid c => ExplInit IO (Global c) where
  {-# INLINE explInit #-}
  explInit = Global <$> newIORef mempty

instance ExplGet IO (Global c) where
  {-# INLINE explGet #-}
  explGet (Global ref) _ = readIORef ref
  {-# INLINE explExists #-}
  explExists _ _ = return True

instance ExplSet IO (Global c) where
  {-# INLINE explSet #-}
  explSet (Global ref) _ c = writeIORef ref c

-- | An empty type class indicating that the store behaves like a regular map, and can therefore safely be cached.
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
--   The cache uses entity (-2) to internally represent missing entities, so be wary when manually manipulating entities.
data Cache (n :: Nat) s =
  Cache Int (UM.IOVector Int) (VM.IOVector (Elem s)) s

cacheMiss :: t
cacheMiss = error "Cache miss!"

type instance Elem (Cache n s) = Elem s

instance (ExplInit IO s, KnownNat n, Cachable s) => ExplInit IO (Cache n s) where
  {-# INLINE explInit #-}
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- UM.replicate n (-2)
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
    when (tag /= (-2) && tag /= ety) $ do
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
         UM.unsafeWrite tags  index (-2)
         VM.unsafeWrite cache index cacheMiss
       else explDestroy s ety

instance ExplMembers IO s => ExplMembers IO (Cache n s) where
  {-# INLINE explMembers #-}
  explMembers (Cache _ tags _ s) = do
    cached <- U.filter (/= (-2)) <$> U.freeze tags
    stored <- explMembers s
    return $! cached U.++ stored

data Register s = Register (VM.IOVector S.IntSet) s
type instance Elem (Register s) = Elem s

instance (Cachable s, ExplInit IO s, Bounded (Elem s), Enum (Elem s)) => ExplInit IO (Register s) where
  explInit = do
    vec <- VM.replicate
             (fromEnum (maxBound :: Elem s) - fromEnum (minBound :: Elem s) + 1)
             mempty
    s <- explInit
    return $ Register vec s

instance ExplGet m s => ExplGet m (Register s) where
  {-# INLINE explGet #-}
  explExists (Register _ s) e = explExists s e
  explGet (Register _ s) e = explGet s e

instance (ExplGet IO s, ExplSet IO s, Bounded (Elem s), Enum (Elem s)) => ExplSet IO (Register s) where
  explSet (Register vec s) ety x = do
    let offset = negate $ fromEnum (minBound :: Elem s)
    ex <- explExists s ety
    when ex $ do
      xOld <- explGet s ety
      VM.modify vec (S.delete ety) (fromEnum xOld - offset)
    VM.modify vec (S.insert ety) (fromEnum x - offset)
    explSet s ety x

instance (ExplGet IO s, ExplDestroy IO s, Bounded (Elem s), Enum (Elem s)) => ExplDestroy IO (Register s) where
  explDestroy (Register vec s) ety = do
    let offset = negate $ fromEnum (minBound :: Elem s)
    ex <- explExists s ety
    when ex $ do
      xOld <- explGet s ety
      VM.modify vec (S.delete ety) (fromEnum xOld - offset)
    explDestroy s ety

instance ExplMembers m s => ExplMembers m (Register s) where
  {-# INLINE explMembers #-}
  explMembers (Register _ s) = explMembers s

regLookup :: forall w s c.
  ( Component c, Bounded c, Enum c
  , Has w IO c
  , Storage c ~ Register s
  , c ~ Elem s
  )
  => c -> System w [Entity]
regLookup c = do
  let offset = negate $ fromEnum (minBound :: Elem s)
  Register vec _ :: Register s <- getStore
  fmap Entity . S.toList <$> lift (VM.read vec (fromEnum c - offset))
