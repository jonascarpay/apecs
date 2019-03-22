{-|
Stability : experimental

This module is experimental, and its API might change between point releases. Use at your own risk.
--}
module Apecs.Experimental.Util
  ( -- * Spatial hashing
    -- $hash
  quantize, flatten, inbounds, region, flatten',
  ) where

import Control.Applicative  (liftA2)

-- $hash
-- The following are helper functions for spatial hashing.
-- Your spatial hash is defined by two vectors;
--
--   - The cell size vector contains real components and dictates
--     how large each cell in your table is in world space units.
--     It is used by @quantize@ to translate a world space coordinate into a table space index vector
--   - The table size vector contains integral components and dictates how
--     many cells your field consists of in each direction.
--     It is used by @flatten@ to translate a table-space index vector into a flat integer

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
flatten size vec = if inbounds size vec then Just (flatten' size vec) else Nothing

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

-- | flatten, but yields garbage for out-of-bounds vectors.
{-# INLINE flatten' #-}
flatten' :: (Applicative v, Integral a, Foldable v)
            => v a -- Field size vector
            -> v a -> a
flatten' size vec = foldr (\(n,x) acc -> n*acc + x) 0 (liftA2 (,) size vec)
