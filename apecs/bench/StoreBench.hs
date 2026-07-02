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
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Linear (V2 (..), V4 (..))

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

-- Cache over Map: size 1024 (< n = 10000) so eviction occurs in all patterns
newtype CInt1024 = CInt1024 Int
instance Component CInt1024 where type Storage CInt1024 = Cache 1024 (Map CInt1024)
makeWorld "Cache1024World" [''CInt1024]
instance NFData Cache1024World where rnf (Cache1024World _ _) = ()

-- ArrayMapB: boxed direct-indexed growing array
newtype BAInt = BAInt Int
instance Component BAInt where type Storage BAInt = ArrayMapB BAInt
makeWorld "ArrayWorldB" [''BAInt]
instance NFData ArrayWorldB where rnf (ArrayWorldB _ _) = ()

-- ArrayMapU: unboxed direct-indexed growing array (Int has Unbox)
instance Component Int where type Storage Int = ArrayMapU Int
makeWorld "ArrayWorldU" [''Int]
instance NFData ArrayWorldU where rnf (ArrayWorldU _ _) = ()

-- ChunkStoreB: boxed paged store, page size 64 matches chunkSize
newtype BSInt = BSInt Int
instance Component BSInt where type Storage BSInt = ChunkStoreB 64 BSInt
makeWorld "ChunkWorldB" [''BSInt]
instance NFData ChunkWorldB where rnf (ChunkWorldB _ _) = ()

-- ChunkStoreU: unboxed paged store, page size 64 (Double has Unbox)
instance Component Double where type Storage Double = ChunkStoreU 64 Double
makeWorld "ChunkWorldU" [''Double]
instance NFData ChunkWorldU where rnf (ChunkWorldU _ _) = ()

-- SparseStoreB: boxed sparse-set (dense packed array + sparse index)
newtype BSSInt = BSSInt Int
instance Component BSSInt where type Storage BSSInt = SparseStoreB BSSInt
makeWorld "SparseWorldB" [''BSSInt]
instance NFData SparseWorldB where rnf (SparseWorldB _ _) = ()

-- SparseStoreU: unboxed sparse-set (Word has Unbox; Int and Double already used above)
instance Component Word where type Storage Word = SparseStoreU Word
makeWorld "SparseWorldU" [''Word]
instance NFData SparseWorldU where rnf (SparseWorldU _ _) = ()

-- ---------------------------------------------------------------------------
-- Item-size comparison: V4 Double (32B) and V4 (V2 Double) (64B)
-- Each (store, size) pair needs a distinct newtype to allow separate Component instances.
-- Unboxed variants use derivingUnbox to delegate to the underlying linear type's Unbox.

-- 32-byte component (V4 Double) --

newtype BA32 = BA32 (V4 Double)
instance Component BA32 where type Storage BA32 = ArrayMapB BA32
makeWorld "BA32World" [''BA32]
instance NFData BA32World where rnf (BA32World _ _) = ()

newtype UA32 = UA32 (V4 Double)
$(derivingUnbox "UA32" [t| UA32 -> V4 Double |] [| \(UA32 v) -> v |] [| UA32 |])
instance Component UA32 where type Storage UA32 = ArrayMapU UA32
makeWorld "UA32World" [''UA32]
instance NFData UA32World where rnf (UA32World _ _) = ()

newtype BS32 = BS32 (V4 Double)
instance Component BS32 where type Storage BS32 = SparseStoreB BS32
makeWorld "BS32World" [''BS32]
instance NFData BS32World where rnf (BS32World _ _) = ()

newtype US32 = US32 (V4 Double)
$(derivingUnbox "US32" [t| US32 -> V4 Double |] [| \(US32 v) -> v |] [| US32 |])
instance Component US32 where type Storage US32 = SparseStoreU US32
makeWorld "US32World" [''US32]
instance NFData US32World where rnf (US32World _ _) = ()

-- 64-byte component (V4 (V2 Double)) --

newtype BA64 = BA64 (V4 (V2 Double))
instance Component BA64 where type Storage BA64 = ArrayMapB BA64
makeWorld "BA64World" [''BA64]
instance NFData BA64World where rnf (BA64World _ _) = ()

newtype UA64 = UA64 (V4 (V2 Double))
$(derivingUnbox "UA64" [t| UA64 -> V4 (V2 Double) |] [| \(UA64 v) -> v |] [| UA64 |])
instance Component UA64 where type Storage UA64 = ArrayMapU UA64
makeWorld "UA64World" [''UA64]
instance NFData UA64World where rnf (UA64World _ _) = ()

newtype BS64 = BS64 (V4 (V2 Double))
instance Component BS64 where type Storage BS64 = SparseStoreB BS64
makeWorld "BS64World" [''BS64]
instance NFData BS64World where rnf (BS64World _ _) = ()

newtype US64 = US64 (V4 (V2 Double))
$(derivingUnbox "US64" [t| US64 -> V4 (V2 Double) |] [| \(US64 v) -> v |] [| US64 |])
instance Component US64 where type Storage US64 = USparseStore US64
makeWorld "US64World" [''US64]
instance NFData US64World where rnf (US64World _ _) = ()

-- Helpers for building and reading large component values
mk32 :: Int -> V4 Double
mk32 i = V4 (fromIntegral i) 0 0 0

rd32 :: V4 Double -> Int
rd32 (V4 x _ _ _) = round x

mk64 :: Int -> V4 (V2 Double)
mk64 i = V4 (V2 (fromIntegral i) 0) (V2 0 0) (V2 0 0) (V2 0 0)

rd64 :: V4 (V2 Double) -> Int
rd64 (V4 (V2 x _) _ _ _) = round x

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
  [ bench "Map"          $ whnfIO (writeBench initMapWorld       (\i -> set (Entity i) (MInt i))                                    indices)
  , bench "Cache-16384"  $ whnfIO (writeBench initCacheWorld     (\i -> set (Entity i) (CInt i))                                    indices)
  , bench "Cache-1024"   $ whnfIO (writeBench initCache1024World (\i -> set (Entity i) (CInt1024 i))                               indices)
  , bench "ArrayMapB"    $ whnfIO (writeBench initArrayWorldB (\i -> set (Entity i) (BAInt i))                                   indices)
  , bench "ArrayMapU"    $ whnfIO (writeBench initArrayWorldU (\i -> set (Entity i) i)                                           indices)
  , bench "ChunkStoreB"   $ whnfIO (writeBench initChunkWorldB  (\i -> set (Entity i) (BSInt i))                                  indices)
  , bench "ChunkStoreU"   $ whnfIO (writeBench initChunkWorldU  (\i -> set (Entity i) (fromIntegral i :: Double))                 indices)
  , bench "SparseStoreB"     $ whnfIO (writeBench initSparseWorldB (\i -> set (Entity i) (BSSInt i))                      indices)
  , bench "SparseStoreU"     $ whnfIO (writeBench initSparseWorldU (\i -> set (Entity i) (fromIntegral i :: Word))        indices)
  ]

writeGroupLarge :: [Int] -> [Benchmark]
writeGroupLarge indices =
  [ bench "ArrayMapB-32B"    $ whnfIO (writeBench initBA32World    (\i -> set (Entity i) (BA32 (mk32 i)))                 indices)
  , bench "ArrayMapU-32B"    $ whnfIO (writeBench initUA32World    (\i -> set (Entity i) (UA32 (mk32 i)))                 indices)
  , bench "SparseStoreB-32B" $ whnfIO (writeBench initBS32World    (\i -> set (Entity i) (BS32 (mk32 i)))                 indices)
  , bench "SparseStoreU-32B" $ whnfIO (writeBench initUS32World    (\i -> set (Entity i) (US32 (mk32 i)))                 indices)
  , bench "ArrayMapB-64B"    $ whnfIO (writeBench initBA64World    (\i -> set (Entity i) (BA64 (mk64 i)))                 indices)
  , bench "ArrayMapU-64B"    $ whnfIO (writeBench initUA64World    (\i -> set (Entity i) (UA64 (mk64 i)))                 indices)
  , bench "SparseStoreB-64B" $ whnfIO (writeBench initBS64World    (\i -> set (Entity i) (BS64 (mk64 i)))                 indices)
  , bench "SparseStoreU-64B" $ whnfIO (writeBench initUS64World    (\i -> set (Entity i) (US64 (mk64 i)))                 indices)
  ]

readGroup :: [Int] -> [Benchmark]
readGroup indices =
  [ namedReadBench "Map"         initMapWorld       (\i -> set (Entity i) (MInt i))     (\i -> (\(MInt x) -> x) <$> get (Entity i))         indices
  , namedReadBench "Cache-16384" initCacheWorld     (\i -> set (Entity i) (CInt i))     (\i -> (\(CInt x) -> x) <$> get (Entity i))         indices
  , namedReadBench "Cache-1024"  initCache1024World (\i -> set (Entity i) (CInt1024 i)) (\i -> (\(CInt1024 x) -> x) <$> get (Entity i))     indices
  , namedReadBench "ArrayMapB"   initArrayWorldB (\i -> set (Entity i) (BAInt i)) (\i -> (\(BAInt x) -> x) <$> get (Entity i))  indices
  , namedReadBench "ArrayMapU"   initArrayWorldU (\i -> set (Entity i) i)         (\i -> get (Entity i))                         indices
  , namedReadBench "ChunkStoreB"  initChunkWorldB  (\i -> set (Entity i) (BSInt i))                    (\i -> (\(BSInt x) -> x) <$> get (Entity i))                           indices
  , namedReadBench "ChunkStoreU"  initChunkWorldU  (\i -> set (Entity i) (fromIntegral i :: Double))   (\i -> round <$> (get (Entity i) :: System ChunkWorldU Double))           indices
  , namedReadBench "SparseStoreB"     initSparseWorldB (\i -> set (Entity i) (BSSInt i))                   (\i -> (\(BSSInt x) -> x) <$> get (Entity i))                                                 indices
  , namedReadBench "SparseStoreU"     initSparseWorldU (\i -> set (Entity i) (fromIntegral i :: Word))     (\i -> fromIntegral <$> (get (Entity i) :: System SparseWorldU Word))                       indices
  ]

readGroupLarge :: [Int] -> [Benchmark]
readGroupLarge indices =
  [ namedReadBench "ArrayMapB-32B"    initBA32World    (\i -> set (Entity i) (BA32 (mk32 i)))              (\i -> (\(BA32 v) -> rd32 v) <$> get (Entity i))                                           indices
  , namedReadBench "ArrayMapU-32B"    initUA32World    (\i -> set (Entity i) (UA32 (mk32 i)))              (\i -> (\(UA32 v) -> rd32 v) <$> get (Entity i))                                           indices
  , namedReadBench "SparseStoreB-32B" initBS32World    (\i -> set (Entity i) (BS32 (mk32 i)))              (\i -> (\(BS32 v) -> rd32 v) <$> get (Entity i))                                           indices
  , namedReadBench "SparseStoreU-32B" initUS32World    (\i -> set (Entity i) (US32 (mk32 i)))              (\i -> (\(US32 v) -> rd32 v) <$> get (Entity i))                                           indices
  , namedReadBench "ArrayMapB-64B"    initBA64World    (\i -> set (Entity i) (BA64 (mk64 i)))              (\i -> (\(BA64 v) -> rd64 v) <$> get (Entity i))                                           indices
  , namedReadBench "ArrayMapU-64B"    initUA64World    (\i -> set (Entity i) (UA64 (mk64 i)))              (\i -> (\(UA64 v) -> rd64 v) <$> get (Entity i))                                           indices
  , namedReadBench "SparseStoreB-64B" initBS64World    (\i -> set (Entity i) (BS64 (mk64 i)))              (\i -> (\(BS64 v) -> rd64 v) <$> get (Entity i))                                           indices
  , namedReadBench "SparseStoreU-64B" initUS64World    (\i -> set (Entity i) (US64 (mk64 i)))              (\i -> (\(US64 v) -> rd64 v) <$> get (Entity i))                                           indices
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
  , bgroup "large"
    [ bgroup "linear"
      [ bgroup "write" (writeGroupLarge linearIndices)
      , bgroup "read"  (readGroupLarge  linearIndices)
      ]
    , bgroup "chunked"
      [ bgroup "write" (writeGroupLarge chunkedIndices)
      , bgroup "read"  (readGroupLarge  chunkedIndices)
      ]
      , bgroup "random"
      [ bgroup "write" (writeGroupLarge shuffledIndices)
      , bgroup "read"  (readGroupLarge  shuffledIndices)
      ]
    ]
  ]
