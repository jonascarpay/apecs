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
import Criterion.Main
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector.Unboxed.Deriving (derivingUnbox)

import Apecs
import Apecs.Experimental.ArrayMap
import Apecs.Experimental.ChunkStore
import Apecs.Experimental.SparseStore

-- ---------------------------------------------------------------------------
-- Parameters

n :: Int
n = 10000

linearIndices, shuffledIndices :: [Int]
linearIndices   = [0 .. n - 1]
shuffledIndices = sortBy (comparing lcgKey) [0 .. n - 1]
  where lcgKey x = x * 6364136223846793005 + 1442695040888963407 :: Int

-- ---------------------------------------------------------------------------
-- World: one component per store type, all newtypes over Int for uniform size.
--
-- WTarget is the fixed "second" component in every cmap benchmark.  It is
-- backed by ArrayMapU — a dense contiguous array — so sequential vs random
-- access into it is clearly visible in the numbers.
--
-- Insertion order (linear vs shuffled) controls SparseStore's dense-array
-- layout, and therefore the entity order returned by its explMembers.
-- All other stores (Map, Cache, ArrayMap, ChunkStore) always return entities
-- in sorted entity-ID order regardless of insertion order.

newtype WMap    = WMap    Int
newtype WCache  = WCache  Int
newtype WArray  = WArray  Int
newtype WChunk  = WChunk  Int
newtype WSparse = WSparse Int
newtype WTarget = WTarget Int

$(derivingUnbox "WArray"  [t| WArray  -> Int |] [| \(WArray  i) -> i |] [| WArray  |])
$(derivingUnbox "WSparse" [t| WSparse -> Int |] [| \(WSparse i) -> i |] [| WSparse |])
$(derivingUnbox "WTarget" [t| WTarget -> Int |] [| \(WTarget i) -> i |] [| WTarget |])

instance Component WMap    where type Storage WMap    = Map WMap
instance Component WCache  where type Storage WCache  = Cache 16384 (Map WCache)
instance Component WArray  where type Storage WArray  = ArrayMapU WArray
instance Component WChunk  where type Storage WChunk  = ChunkStoreB 64 WChunk
instance Component WSparse where type Storage WSparse = SparseStoreU WSparse
instance Component WTarget where type Storage WTarget = ArrayMapU WTarget

makeWorld "MultiWorld" [''WMap, ''WCache, ''WArray, ''WChunk, ''WSparse, ''WTarget]
instance NFData MultiWorld where rnf (MultiWorld _ _ _ _ _ _ _) = ()

-- ---------------------------------------------------------------------------
-- Helpers

populate :: [Int] -> IO MultiWorld
populate indices = do
  w <- initMultiWorld
  runSystem (mapM_ write indices) w
  pure w
  where
    write i = do
      set (Entity i) (WMap    i)
      set (Entity i) (WCache  i)
      set (Entity i) (WArray  i)
      set (Entity i) (WChunk  i)
      set (Entity i) (WSparse i)
      set (Entity i) (WTarget i)

-- ---------------------------------------------------------------------------
-- Benchmarks

-- | For each store type as the iteration driver (first component), read both
-- the first and WTarget components and sum their values.  The sum forces all
-- reads; whnfIO on an Int is enough to evaluate the result fully.
readFirstGroup :: String -> [Int] -> Benchmark
readFirstGroup label indices =
  env (populate indices) $ \w ->
    bgroup label
      [ bench "Map-first"    $ whnfIO $ runSystem (cfold (\acc (WMap    a, WTarget b) -> acc + a + b) (0 :: Int)) w
      , bench "Cache-first"  $ whnfIO $ runSystem (cfold (\acc (WCache  a, WTarget b) -> acc + a + b) (0 :: Int)) w
      , bench "Array-first"  $ whnfIO $ runSystem (cfold (\acc (WArray  a, WTarget b) -> acc + a + b) (0 :: Int)) w
      , bench "Chunk-first"  $ whnfIO $ runSystem (cfold (\acc (WChunk  a, WTarget b) -> acc + a + b) (0 :: Int)) w
      , bench "Sparse-first" $ whnfIO $ runSystem (cfold (\acc (WSparse a, WTarget b) -> acc + a + b) (0 :: Int)) w
      ]

-- | Iterate via each first store and write back to WTarget.  Measures the
-- combined cost of iteration order + cross-store write; directly comparable
-- to readFirstGroup since only the cfold→cmap direction changes.
writeFirstGroup :: String -> [Int] -> Benchmark
writeFirstGroup label indices =
  env (populate indices) $ \w ->
    bgroup label
      [ bench "Map-first"    $ whnfIO $ runSystem (cmap (\(WMap    a, WTarget b) -> WTarget (a + b))) w
      , bench "Cache-first"  $ whnfIO $ runSystem (cmap (\(WCache  a, WTarget b) -> WTarget (a + b))) w
      , bench "Array-first"  $ whnfIO $ runSystem (cmap (\(WArray  a, WTarget b) -> WTarget (a + b))) w
      , bench "Chunk-first"  $ whnfIO $ runSystem (cmap (\(WChunk  a, WTarget b) -> WTarget (a + b))) w
      , bench "Sparse-first" $ whnfIO $ runSystem (cmap (\(WSparse a, WTarget b) -> WTarget (a + b))) w
      ]

-- | Each store iterates its own components and writes back to itself.
-- Accesses are always in the store's natural order, so this is purely a
-- write-throughput baseline with no cross-store or random-access effects.
writeSelfGroup :: String -> [Int] -> Benchmark
writeSelfGroup label indices =
  env (populate indices) $ \w ->
    bgroup label
      [ bench "Map"    $ whnfIO $ runSystem (cmap (\(WMap    a) -> WMap    (a + 1))) w
      , bench "Cache"  $ whnfIO $ runSystem (cmap (\(WCache  a) -> WCache  (a + 1))) w
      , bench "Array"  $ whnfIO $ runSystem (cmap (\(WArray  a) -> WArray  (a + 1))) w
      , bench "Chunk"  $ whnfIO $ runSystem (cmap (\(WChunk  a) -> WChunk  (a + 1))) w
      , bench "Sparse" $ whnfIO $ runSystem (cmap (\(WSparse a) -> WSparse (a + 1))) w
      ]

main :: IO ()
main = defaultMain
  [ bgroup "linear"
    [ readFirstGroup  "read"       linearIndices
    , writeFirstGroup "write"      linearIndices
    , writeSelfGroup  "write-self" linearIndices
    ]
  , bgroup "shuffled"
    [ readFirstGroup  "read"       shuffledIndices
    , writeFirstGroup "write"      shuffledIndices
    , writeSelfGroup  "write-self" shuffledIndices
    ]
  ]
