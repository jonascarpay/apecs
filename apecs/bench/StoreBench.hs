{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Criterion.Main
import Data.List (sortBy)
import Data.Ord (comparing)

import Apecs
import Apecs.Experimental.ArrayMap
import Apecs.Experimental.ChunkStore
import Apecs.Experimental.SparseStore

-- ---------------------------------------------------------------------------
-- Parameters

n :: Int
n = 10000

-- 64-entity chunks, stride 256 (192-entity gap between chunks).
-- With ChunkStore page size 64, each chunk maps to exactly one page.
-- With ArrayMap (chunked), entity IDs reach ~(n/64)*256 ~ 40000,
-- showing the wasted capacity vs ChunkStore.
chunkSize, chunkStride :: Int
chunkSize   = 64
chunkStride = 256

linearIndices, chunkedIndices, shuffledIndices :: [Int]
linearIndices  = [0 .. n - 1]
chunkedIndices = take n [b + o | b <- [0, chunkStride ..], o <- [0 .. chunkSize - 1]]
shuffledIndices = sortBy (comparing lcgKey) [0 .. n - 1]
  where
    -- Knuth multiplicative hash — deterministic shuffle of [0..n-1]
    lcgKey x = x * 6364136223846793005 + 1442695040888963407 :: Int

-- ---------------------------------------------------------------------------
-- Stores and worlds

-- Map: baseline key/value store backed by IntMap
newtype MInt = MInt Int
instance Component MInt where type Storage MInt = Map MInt
makeWorld "MapWorld" [''MInt]
instance NFData MapWorld where rnf (MapWorld _ _) = ()

-- Cache over Map: size 16384 (≥ n) so no eviction occurs in these benchmarks
newtype CInt = CInt Int
instance Component CInt where type Storage CInt = Cache 16384 (Map CInt)
makeWorld "CacheWorld" [''CInt]
instance NFData CacheWorld where rnf (CacheWorld _ _) = ()

-- BArrayMap: boxed direct-indexed growing array
newtype BAInt = BAInt Int
instance Component BAInt where type Storage BAInt = BArrayMap BAInt
makeWorld "BArrayWorld" [''BAInt]
instance NFData BArrayWorld where rnf (BArrayWorld _ _) = ()

-- UArrayMap: unboxed direct-indexed growing array (Int has Unbox)
instance Component Int where type Storage Int = UArrayMap Int
makeWorld "UArrayWorld" [''Int]
instance NFData UArrayWorld where rnf (UArrayWorld _ _) = ()

-- BChunkStore: boxed paged store, page size 64 matches chunkSize
newtype BSInt = BSInt Int
instance Component BSInt where type Storage BSInt = BChunkStore 64 BSInt
makeWorld "BChunkWorld" [''BSInt]
instance NFData BChunkWorld where rnf (BChunkWorld _ _) = ()

-- UChunkStore: unboxed paged store, page size 64 (Double has Unbox)
instance Component Double where type Storage Double = UChunkStore 64 Double
makeWorld "UChunkWorld" [''Double]
instance NFData UChunkWorld where rnf (UChunkWorld _ _) = ()

-- BSparseStore: boxed sparse-set (dense packed array + sparse index)
newtype BSSInt = BSSInt Int
instance Component BSSInt where type Storage BSSInt = BSparseStore BSSInt
makeWorld "BSparseWorld" [''BSSInt]
instance NFData BSparseWorld where rnf (BSparseWorld _ _) = ()

-- USparseStore: unboxed sparse-set (Word has Unbox; Int and Double already used above)
instance Component Word where type Storage Word = USparseStore Word
makeWorld "USparseWorld" [''Word]
instance NFData USparseWorld where rnf (USparseWorld _ _) = ()

-- ---------------------------------------------------------------------------
-- Benchmark helpers

-- | Write all indices in a fresh world each run (cold path: includes init and
-- any array growth).
writeBench :: IO w -> (Int -> System w ()) -> [Int] -> IO ()
writeBench initW wr indices = do
  w <- initW
  runSystem (mapM_ wr indices) w

-- | Pre-populate a world once (outside Criterion timing via 'env'), then read
-- every index each run, summing results to force all reads.
namedReadBench
  :: NFData w
  => String
  -> IO w
  -> (Int -> System w ())
  -> (Int -> System w Int)
  -> [Int]
  -> Benchmark
namedReadBench name initW wr rd indices =
  env populate $ \w ->
    bench name $
      whnfIO (runSystem (foldM (\acc i -> (acc +) <$> rd i) (0 :: Int) indices) w)
  where
    populate = do
      w <- initW
      runSystem (mapM_ wr indices) w
      pure w

-- ---------------------------------------------------------------------------
-- Per-pattern benchmark groups: one [Benchmark] per store, compared side by side.

writeGroup :: [Int] -> [Benchmark]
writeGroup indices =
  [ bench "Map"          $ whnfIO (writeBench initMapWorld    (\i -> set (Entity i) (MInt i))                                    indices)
  , bench "Cache-16384"  $ whnfIO (writeBench initCacheWorld  (\i -> set (Entity i) (CInt i))                                    indices)
  , bench "BArrayMap"    $ whnfIO (writeBench initBArrayWorld (\i -> set (Entity i) (BAInt i))                                   indices)
  , bench "UArrayMap"    $ whnfIO (writeBench initUArrayWorld (\i -> set (Entity i) i)                                           indices)
  , bench "BChunkStore"   $ whnfIO (writeBench initBChunkWorld  (\i -> set (Entity i) (BSInt i))                                  indices)
  , bench "UChunkStore"   $ whnfIO (writeBench initUChunkWorld  (\i -> set (Entity i) (fromIntegral i :: Double))                 indices)
  , bench "BSparseStore"  $ whnfIO (writeBench initBSparseWorld (\i -> set (Entity i) (BSSInt i))                                 indices)
  , bench "USparseStore"  $ whnfIO (writeBench initUSparseWorld (\i -> set (Entity i) (fromIntegral i :: Word))                   indices)
  ]

readGroup :: [Int] -> [Benchmark]
readGroup indices =
  [ namedReadBench "Map"         initMapWorld    (\i -> set (Entity i) (MInt i))  (\i -> (\(MInt x) -> x) <$> get (Entity i))   indices
  , namedReadBench "Cache-16384" initCacheWorld  (\i -> set (Entity i) (CInt i))  (\i -> (\(CInt x) -> x) <$> get (Entity i))   indices
  , namedReadBench "BArrayMap"   initBArrayWorld (\i -> set (Entity i) (BAInt i)) (\i -> (\(BAInt x) -> x) <$> get (Entity i))  indices
  , namedReadBench "UArrayMap"   initUArrayWorld (\i -> set (Entity i) i)         (\i -> get (Entity i))                         indices
  , namedReadBench "BChunkStore"  initBChunkWorld  (\i -> set (Entity i) (BSInt i))                    (\i -> (\(BSInt x) -> x) <$> get (Entity i))                           indices
  , namedReadBench "UChunkStore"  initUChunkWorld  (\i -> set (Entity i) (fromIntegral i :: Double))   (\i -> round <$> (get (Entity i) :: System UChunkWorld Double))           indices
  , namedReadBench "BSparseStore" initBSparseWorld (\i -> set (Entity i) (BSSInt i))                   (\i -> (\(BSSInt x) -> x) <$> get (Entity i))                            indices
  , namedReadBench "USparseStore" initUSparseWorld (\i -> set (Entity i) (fromIntegral i :: Word))     (\i -> fromIntegral <$> (get (Entity i) :: System USparseWorld Word))     indices
  ]

-- ---------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "linear"
    [ bgroup "write" (writeGroup linearIndices)
    , bgroup "read"  (readGroup  linearIndices)
    ]
  , bgroup "chunked"
    [ bgroup "write" (writeGroup chunkedIndices)
    , bgroup "read"  (readGroup  chunkedIndices)
    ]
  , bgroup "random"
    [ bgroup "write" (writeGroup shuffledIndices)
    , bgroup "read"  (readGroup  shuffledIndices)
    ]
  ]
