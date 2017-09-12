{-# LANGUAGE Strict, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Apecs.Util (
  -- * Utility
  initStore, ConcatQueries(..), runGC,

  -- * EntityCounter
  EntityCounter, initCounter, nextEntity, newEntity,

  -- * Spatial hashing
  quantize, flatten, region, inbounds,

  -- * Optimized maps
  rmap', rmap, wmap, wmap', cmap',

  -- * Slice interation
  sliceForM, sliceForM_, sliceForMC, sliceForMC_,
  sliceMapM, sliceMapM_, sliceMapMC, sliceMapMC_,

  -- * Timing
  timeSystem, timeSystem_,

  ) where

import System.Mem (performMajorGC)
import Control.Monad.Reader (liftIO)
import Control.Applicative (liftA2)
import qualified Data.Vector.Unboxed as U
import Data.Traversable (for)
import System.CPUTime

import Apecs.Core
import Apecs.Stores

-- | Initializes a store with (), useful since most stores have () as their initialization argument
initStore :: (Initializable s, InitArgs s ~ ()) => IO s
initStore = initStoreWith ()

newtype EntityCounter = EntityCounter Int
instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

initCounter :: IO (Storage EntityCounter)
initCounter = initStoreWith (EntityCounter 0)

{-# INLINE nextEntity #-}
nextEntity :: Has w EntityCounter => System w (Entity ())
nextEntity = do EntityCounter n <- readGlobal
                writeGlobal (EntityCounter (n+1))
                return (Entity n)

{-# INLINE newEntity #-}
newEntity :: (IsRuntime c, Has w c, Has w EntityCounter)
          => c -> System w (Entity c)
newEntity c = do ety <- nextEntity
                 set (cast ety) c
                 return (cast ety)

runGC :: System w ()
runGC = liftIO performMajorGC

newtype ConcatQueries q = ConcatQueries [q]
instance Query q s => Query (ConcatQueries q) s where
  explSlice s (ConcatQueries qs) = mconcat <$> traverse (explSlice s) qs

cmap' :: forall world c. (Has world c, IsRuntime c)
      => (c -> Safe c) -> System world ()
cmap' f = do s :: Storage c <- getStore
             liftIO$ do sl <- explMembers s
                        U.forM_ sl $ \e -> do
                          r <- explGetUnsafe s e
                          explSetMaybe s e (getSafe . f $ r)

-- | Maps a function over all entities with a @r@, and writes their @w@
{-# INLINE rmap #-}
rmap :: forall world r w. (Has world w, Has world r, IsRuntime w, IsRuntime r)
      => (r -> w) -> System world ()
rmap f = do sr :: Storage r <- getStore
            sc :: Storage w <- getStore
            liftIO$ do sl <- explMembers sr
                       U.forM_ sl $ \ e -> do
                          r <- explGetUnsafe sr e
                          explSet sc e (f r)

-- | Maps a function over all entities with a @r@, and writes or deletes their @w@
{-# INLINE rmap' #-}
rmap' :: forall world r w. (Has world w, Has world r, Store (Storage w), IsRuntime r)
      => (r -> Safe w) -> System world ()
rmap' f = do sr :: Storage r <- getStore
             sw :: Storage w <- getStore
             liftIO$ do sl <- explMembers sr
                        U.forM_ sl $ \ e -> do
                           r <- explGetUnsafe sr e
                           explSetMaybe sw e (getSafe $ f r)

-- | For all entities with a @w@, this map reads their @r@ and writes their @w@
{-# INLINE wmap #-}
wmap :: forall world r w. (Has world w, Has world r, IsRuntime w, IsRuntime r)
     => (Safe r -> w) -> System world ()
wmap f = do sr :: Storage r <- getStore
            sw :: Storage w <- getStore
            liftIO$ do sl <- explMembers sr
                       U.forM_ sl $ \ e -> do
                         r <- explGet sr e
                         explSet sw e (f . Safe $ r)

-- | For all entities with a @w@, this map reads their @r@ and writes or deletes their @w@
{-# INLINE wmap' #-}
wmap' :: forall world r w. (Has world w, Has world r, Store (Storage w), IsRuntime r)
      => (Safe r -> Safe w) -> System world ()
wmap' f = do sr :: Storage r <- getStore
             sw :: Storage w <- getStore
             liftIO$ do sl <- explMembers sr
                        U.forM_ sl $ \ e -> do
                          r <- explGet sr e
                          explSetMaybe sw e (getSafe . f . Safe $ r)


-- Slice traversal
{-# INLINE sliceForM_ #-}
sliceForM_ :: Monad m => Slice c -> (Entity c -> m b) -> m ()
sliceForM_ (Slice vec) ma = U.forM_ vec (ma . Entity)

{-# INLINE sliceForM #-}
sliceForM :: Monad m => Slice c -> (Entity c -> m a) -> m [a]
sliceForM (Slice vec) ma = traverse (ma . Entity) (U.toList vec)

{-# INLINE sliceForMC #-}
sliceForMC :: forall w c a. (Store (Storage c), Has w c) => Slice c -> ((Entity c,Safe c) -> System w a) -> System w [a]
sliceForMC (Slice vec) sys = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

{-# INLINE sliceForMC_ #-}
sliceForMC_ :: forall w c a. (Store (Storage c), Has w c) => Slice c -> ((Entity c,Safe c) -> System w a) -> System w ()
sliceForMC_ (Slice vec) sys = do
  s :: Storage c <- getStore
  U.forM_ vec $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

{-# INLINE sliceMapM_ #-}
sliceMapM_ :: Monad m => (Entity c -> m a) -> Slice c -> m ()
sliceMapM_ ma (Slice vec) = U.mapM_ (ma . Entity) vec

{-# INLINE sliceMapM #-}
sliceMapM :: Monad m => (Entity c -> m a) -> Slice c -> m [a]
sliceMapM ma (Slice vec) = traverse (ma . Entity) (U.toList vec)

{-# INLINE sliceMapMC #-}
sliceMapMC :: forall w c a. (Store (Storage c), Has w c) => ((Entity c,Safe c) -> System w a) -> Slice c -> System w [a]
sliceMapMC sys (Slice vec) = do
  s :: Storage c <- getStore
  for (U.toList vec) $ \e -> do
    r <- liftIO$ explGet s e
    sys (Entity e, Safe r)

{-# INLINE sliceMapMC_ #-}
sliceMapMC_ :: forall w c a. (Store (Storage c), Has w c) => ((Entity c, Safe c) -> System w a) -> Slice c -> System w ()
sliceMapMC_ sys vec = sliceForMC_ vec sys

-- | The following functions are for spatial hashing.
--   The idea is that your spatial hash is defined by two vectors;
--     - The cell size vector contains real components and dictates
--       how large each cell in your table is spatially.
--       It is used to translate from world-space to table space
--     - The field size vector contains integral components and dictates how
--       many cells your field consists of in each direction.
--       It is used to translate from table-space to a flat integer

-- | Quantize turns a world-space coordinate into a table-space coordinate by dividing
--   by the given cell size and round components towards negative infinity
{-# INLINE quantize #-}
quantize :: (Fractional (v a), Integral b, RealFrac a, Functor v)
         => v a -- ^ Quantization cell size
         -> v a -- ^ Vector to be quantized
         -> v b
quantize cell vec = floor <$> vec/cell

-- | For two table-space vectors indicating a region's bounds, gives a list of the vectors contained between them.
--   This is useful for querying a spatial hash.
{-# INLINE region #-}
region :: (Enum a, Applicative v, Traversable v)
       => v a -- ^ Lower bound for the region
       -> v a -- ^ Higher bound for the region
       -> [v a]
region a b = sequence $ liftA2 enumFromTo a b

-- | Turns a table-space vector into a linear index, given some table size vector.
{-# INLINE flatten #-}
flatten :: (Applicative v, Integral a, Foldable v)
        => v a -- Field size vector
        -> v a -> a
flatten size vec = foldr (\(n,x) acc -> n*acc + x) 0 (liftA2 (,) size vec)

-- | Tests whether a vector is in the region given by 0 and the size vector
{-# INLINE inbounds #-}
inbounds :: (Num (v a), Ord a, Applicative v, Foldable v)
         => v a -> v a -> Bool
inbounds size vec = and (liftA2 (>=) vec 0) && and (liftA2 (<=) vec size)



-- | Runs a system and gives its execution time in seconds
{-# INLINE timeSystem #-}
timeSystem :: System w a -> System w (Double, a)
timeSystem sys = do
  s <- liftIO getCPUTime
  a <- sys
  t <- liftIO getCPUTime
  return (fromIntegral (s-t)/1e12, a)

{-# INLINE timeSystem_ #-}
-- | Runs a system, discards its output, and gives its execution time in seconds
timeSystem_ :: System w a -> System w Double
timeSystem_ = fmap fst . timeSystem
