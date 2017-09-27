{-# LANGUAGE Strict, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Apecs.Util (
  -- * Utility
  initStore, runGC, unEntity,

  -- * EntityCounter
  EntityCounter, nextEntity, newEntity,

  -- * Spatial hashing
  -- $hash
  quantize, flatten, inbounds, region, unsafeFlatten,

  -- * Timing
  timeSystem, timeSystem_,

  ) where

import System.Mem (performMajorGC)
import Control.Monad.Reader (liftIO)
import Control.Applicative (liftA2)
import System.CPUTime
import Data.Monoid

import Apecs.Types
import Apecs.Stores
import Apecs.System

unEntity :: Entity a -> Int
unEntity (Entity e) = e

-- | Secretly just an int in a newtype
newtype EntityCounter = EntityCounter {getCounter :: Sum Int} deriving (Monoid, Num, Eq, Show)

instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

-- | Bumps the EntityCounter and yields its value
{-# INLINE nextEntity #-}
nextEntity :: Has w EntityCounter => System w (Entity ())
nextEntity = do n <- readGlobal
                writeGlobal (n+1)
                return (Entity . getSum . getCounter $ n)

-- | Writes the given components to a new entity, and yields that entity
{-# INLINE newEntity #-}
newEntity :: (Store (Storage c), Has w c, Has w EntityCounter)
          => c -> System w (Entity c)
newEntity c = do ety <- nextEntity
                 set ety c
                 return (cast ety)

-- | Explicitly invoke the garbage collector
runGC :: System w ()
runGC = liftIO performMajorGC

-- $hash
-- The following functions are for spatial hashing.
-- The idea is that your spatial hash is defined by two vectors;
--
--   - The cell size vector contains real components and dictates
--     how large each cell in your table is in world space units.
--     It is used by @quantize@ to translate a world space coordinate into a table space index vector
--   - The table size vector contains integral components and dictates how
--     many cells your field consists of in each direction.
--     It is used by @flatten@ to translate a table-space index vector into a flat integer
--
-- There is currently no dedicated spatial hashing log, but you can use
-- an EnumTable by defining an instance Enum Vec with
-- > fromEnum = flatten size . quantize cell

-- | Quantize turns a world-space coordinate into a table-space coordinate by dividing
--   by the given cell size and rounding towards negative infinity.
{-# INLINE quantize #-}
quantize :: (Fractional (v a), Integral b, RealFrac a, Functor v)
         => v a -- ^ Quantization cell size
         -> v a -- ^ Vector to be quantized
         -> v b
quantize cell vec = floor <$> vec/cell

-- | Turns a table-space vector into an integral index, given some table size vector.
--   Yields Nothing for out-of-bounds queries
{-# INLINE flatten #-}
flatten :: (Applicative v, Integral a, Foldable v)
        => v a -- Field size vector
        -> v a -> Maybe a
flatten size vec = if inbounds size vec then Just (unsafeFlatten size vec) else Nothing

-- | Tests whether a vector is in the region given by 0 and the size vector (inclusive)
{-# INLINE inbounds #-}
inbounds :: (Num a, Ord a, Applicative v, Foldable v)
         => v a -- Field size vector
         -> v a -> Bool
inbounds size vec = and (liftA2 (\v s -> v >= 0 && v <= s) vec size)

-- | For two table-space vectors indicating a region's bounds, gives a list of the vectors contained between them.
--   This is useful for querying a spatial hash.
{-# INLINE region #-}
region :: (Enum a, Applicative v, Traversable v)
       => v a -- ^ Lower bound for the region
       -> v a -- ^ Higher bound for the region
       -> [v a]
region a b = sequence $ liftA2 enumFromTo a b

-- | Unsafe version of flatten. Yields garbage for out-of-bounds queries.
{-# INLINE unsafeFlatten #-}
unsafeFlatten :: (Applicative v, Integral a, Foldable v)
              => v a -- Field size vector
              -> v a -> a
unsafeFlatten size vec = foldr (\(n,x) acc -> n*acc + x) 0 (liftA2 (,) size vec)

-- | Runs a system and gives its execution time in seconds
{-# INLINE timeSystem #-}
timeSystem :: System w a -> System w (Double, a)
timeSystem sys = do
  s <- liftIO getCPUTime
  a <- sys
  t <- liftIO getCPUTime
  return (fromIntegral (t-s)/1e12, a)

{-# INLINE timeSystem_ #-}
-- | Runs a system, discards its output, and gives its execution time in seconds
timeSystem_ :: System w a -> System w Double
timeSystem_ = fmap fst . timeSystem
