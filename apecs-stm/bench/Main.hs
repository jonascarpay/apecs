{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

{-| Benchmarks for apecs-stm.

The single-threaded @init@/@step@ groups run the same pos/vel workload across
four worlds, with vanilla apecs as the reference:

  * vanilla @apecs@: 'Map' stores and the default 'newEntity'.
  * vanilla @apecs@ using 'nextEntityIO': the atomic IO counter path.
  * @apecs-stm@: stm-containers-backed 'STM.Map' stores.
  * @apecs-stm@ tmap: 'STM.TMap' stores (an outer 'TVar' over a per-cell map).
  * @apecs-stm@ sharded: 'Sharded' 'STM.TMap' stores.

The @concurrent@ group scales the number of worker threads as powers of two,
from 2 up to the process capability count, with each worker stepping its own
disjoint slice of entities in many small transactions. It compares the
stm-containers 'STM.Map', the unsharded 'STM.TMap', and the sharded 'STM.TMap',
showing how each behaves under contention.
-}
module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM, replicateM_, void)
import Linear (V2)
import Test.Tasty.Bench

import Apecs
import qualified Apecs.STM as STM
import Apecs.Util (nextEntityIO)

-- Vanilla world -------------------------------------------------------------

newtype Pos = Pos (V2 Float) deriving (Eq, Show)
instance Component Pos where type Storage Pos = Map Pos

newtype Vel = Vel (V2 Float) deriving (Eq, Show)
instance Component Vel where type Storage Vel = Map Vel

makeWorld "Vanilla" [''Pos, ''Vel]

-- STM world (stm-containers Map) --------------------------------------------

newtype SPos = SPos (V2 Float) deriving (Eq, Show)
instance Component SPos where type Storage SPos = STM.Map SPos

newtype SVel = SVel (V2 Float) deriving (Eq, Show)
instance Component SVel where type Storage SVel = STM.Map SVel

STM.makeWorld "Stm" [''SPos, ''SVel]

-- STM world (unsharded TMap) ------------------------------------------------

newtype TPos = TPos (V2 Float) deriving (Eq, Show)
instance Component TPos where type Storage TPos = STM.TMap TPos

newtype TVel = TVel (V2 Float) deriving (Eq, Show)
instance Component TVel where type Storage TVel = STM.TMap TVel

STM.makeWorld "Tmap" [''TPos, ''TVel]

-- STM world (Sharded TMap) --------------------------------------------------

newtype CPos = CPos (V2 Float) deriving (Eq, Show)
instance Component CPos where type Storage CPos = STM.Sharded 256 (STM.TMap CPos)

newtype CVel = CVel (V2 Float) deriving (Eq, Show)
instance Component CVel where type Storage CVel = STM.Sharded 256 (STM.TMap CVel)

STM.makeWorld "ShardedW" [''CPos, ''CVel]

-- Single-threaded workloads -------------------------------------------------

-- | Allocate an entity via the atomic IO counter, mirroring 'newEntity'.
newEntityIO :: (Set w IO c, Has w IO EntityCounter) => c -> SystemT w IO Entity
newEntityIO c = do
  ety <- nextEntityIO
  set ety c
  return ety

vanillaInit :: System Vanilla ()
vanillaInit = do
  replicateM_ 1000 $ void $ newEntity (Pos 0, Vel 1)
  replicateM_ 9000 $ void $ newEntity (Pos 0)

vanillaInitIO :: System Vanilla ()
vanillaInitIO = do
  replicateM_ 1000 $ void $ newEntityIO (Pos 0, Vel 1)
  replicateM_ 9000 $ void $ newEntityIO (Pos 0)

vanillaStep :: System Vanilla ()
vanillaStep = cmap $ \(Vel v, Pos p) -> Pos (p + v)

stmInit :: SystemT Stm STM.STM ()
stmInit = do
  replicateM_ 1000 $ void $ STM.newEntity (SPos 0, SVel 1)
  replicateM_ 9000 $ void $ STM.newEntity (SPos 0)

stmStep :: SystemT Stm STM.STM ()
stmStep = cmap $ \(SVel v, SPos p) -> SPos (p + v)

tmapInit :: SystemT Tmap STM.STM ()
tmapInit = do
  replicateM_ 1000 $ void $ STM.newEntity (TPos 0, TVel 1)
  replicateM_ 9000 $ void $ STM.newEntity (TPos 0)

tmapStep :: SystemT Tmap STM.STM ()
tmapStep = cmap $ \(TVel v, TPos p) -> TPos (p + v)

shardedInit :: SystemT ShardedW STM.STM ()
shardedInit = do
  replicateM_ 1000 $ void $ STM.newEntity (CPos 0, CVel 1)
  replicateM_ 9000 $ void $ STM.newEntity (CPos 0)

shardedStep :: SystemT ShardedW STM.STM ()
shardedStep = cmap $ \(CVel v, CPos p) -> CPos (p + v)

-- Concurrent workload -------------------------------------------------------

-- | Entities owned by each worker.
entsPerWorker :: Int
entsPerWorker = 64

-- | Times each worker sweeps over its entities.
sweeps :: Int
sweeps = 200

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (a, b) = splitAt k xs in a : chunksOf k b

{- | Run @workers@ threads in parallel, each repeatedly stepping its own
disjoint slice of entities, one entity per STM transaction. Returns once all
workers are done.
-}
runConcurrent
  :: w -> [[Entity]] -> (Entity -> SystemT w STM.STM ()) -> IO ()
runConcurrent w buckets step = do
  dones <- mapM (const newEmptyMVar) buckets
  forM_ (zip buckets dones) $ \(es, done) -> forkIO $ do
    replicateM_ sweeps $ forM_ es $ \e -> runSystem (STM.atomically (step e)) w
    putMVar done ()
  mapM_ takeMVar dones

stmStepOne :: Entity -> SystemT Stm STM.STM ()
stmStepOne e = do
  SVel v <- get e
  SPos p <- get e
  set e (SPos (p + v))

tmapStepOne :: Entity -> SystemT Tmap STM.STM ()
tmapStepOne e = do
  TVel v <- get e
  TPos p <- get e
  set e (TPos (p + v))

shardedStepOne :: Entity -> SystemT ShardedW STM.STM ()
shardedStepOne e = do
  CVel v <- get e
  CPos p <- get e
  set e (CPos (p + v))

{- | Build a populated world split into @workers@ buckets of @entsPerWorker@
entities each, using the given world initializer and per-entity constructor.
-}
setupWorld
  :: (Set w STM.STM c, Has w IO STM.EntityCounter)
  => IO w -> c -> Int -> IO (w, [[Entity]])
setupWorld initW c workers = do
  w <- initW
  es <-
    runSystem
      (STM.atomically $ replicateM (workers * entsPerWorker) (STM.newEntity c))
      w
  pure (w, chunksOf entsPerWorker es)

setupStm :: Int -> IO (Stm, [[Entity]])
setupStm = setupWorld initStm (SPos 0, SVel 1)

setupTmap :: Int -> IO (Tmap, [[Entity]])
setupTmap = setupWorld initTmap (TPos 0, TVel 1)

setupSharded :: Int -> IO (ShardedW, [[Entity]])
setupSharded = setupWorld initShardedW (CPos 0, CVel 1)

-- Benchmark tree ------------------------------------------------------------

-- | A @bcompare@ pattern selecting the @leaf@ benchmark inside group @grp@.
vsRef :: String -> String -> String
vsRef grp leaf = "$(NF-1) == \"" ++ grp ++ "\" && $NF == \"" ++ leaf ++ "\""

-- | Pattern selecting the @vanilla@ benchmark within the given group.
vsVanilla :: String -> String
vsVanilla grp = vsRef grp "vanilla"

-- | Pattern selecting the @stm-containers@ benchmark for the given worker count.
vsStmContainers :: Int -> String
vsStmContainers workers = vsRef ("workers=" ++ show workers) "stm-containers"

-- | Worker counts: powers of two from 2 up to the capability count.
workerCounts :: Int -> [Int]
workerCounts caps = takeWhile (<= max 2 caps) (iterate (* 2) 2)

main :: IO ()
main = do
  caps <- getNumCapabilities
  defaultMain
    [ bgroup
        "init"
        [ bench "vanilla" $ whnfIO (initVanilla >>= runSystem vanillaInit)
        , bcompare (vsVanilla "init") $
            bench "vanilla-nextEntityIO" $
              whnfIO (initVanilla >>= runSystem vanillaInitIO)
        , bcompare (vsVanilla "init") $
            bench "apecs-stm" $
              whnfIO (initStm >>= runSystem (STM.atomically stmInit))
        , bcompare (vsVanilla "init") $
            bench "apecs-stm-tmap" $
              whnfIO (initTmap >>= runSystem (STM.atomically tmapInit))
        , bcompare (vsVanilla "init") $
            bench "apecs-stm-sharded" $
              whnfIO (initShardedW >>= runSystem (STM.atomically shardedInit))
        ]
    , bgroup
        "step"
        [ bench "vanilla" $ whnfIO (initVanilla >>= runSystem (vanillaInit >> vanillaStep))
        , bcompare (vsVanilla "step") $
            bench "vanilla-nextEntityIO" $
              whnfIO (initVanilla >>= runSystem (vanillaInitIO >> vanillaStep))
        , bcompare (vsVanilla "step") $
            bench "apecs-stm" $
              whnfIO (initStm >>= runSystem (STM.atomically (stmInit >> stmStep)))
        , bcompare (vsVanilla "step") $
            bench "apecs-stm-tmap" $
              whnfIO (initTmap >>= runSystem (STM.atomically (tmapInit >> tmapStep)))
        , bcompare (vsVanilla "step") $
            bench "apecs-stm-sharded" $
              whnfIO (initShardedW >>= runSystem (STM.atomically (shardedInit >> shardedStep)))
        ]
    , bgroup "concurrent" $
        [ bgroup
            ("workers=" ++ show workers)
            [ bench "stm-containers" $
                whnfIO (setupStm workers >>= \(w, bs) -> runConcurrent w bs stmStepOne)
            , bcompare (vsStmContainers workers) $
                bench "tmap" $
                  whnfIO (setupTmap workers >>= \(w, bs) -> runConcurrent w bs tmapStepOne)
            , bcompare (vsStmContainers workers) $
                bench "sharded-256" $
                  whnfIO (setupSharded workers >>= \(w, bs) -> runConcurrent w bs shardedStepOne)
            ]
        | workers <- workerCounts caps
        ]
    ]
