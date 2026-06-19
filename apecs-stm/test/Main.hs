{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Control.Concurrent.STM as C
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort)
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Apecs (Cache, Map, cfold, destroy, exists, get, global, modify, runSystem, set)
import Apecs.Core
import Apecs.Stores.STM (STM)
import Apecs.TH (makeWorld)
import qualified Apecs.Stores.STM as STM

-- Preamble ------------------------------------------------------------------

instance Arbitrary Entity where
  arbitrary = Entity . getNonNegative <$> arbitrary

-- | Run a @SystemT w IO Bool@ as a QuickCheck property (IO instances).
assertSys :: IO w -> SystemT w IO Bool -> Property
assertSys initW sys = monadicIO $ run (initW >>= runSystem sys) >>= assert

{- | Run a @SystemT w STM Bool@ as a QuickCheck property, driving the STM
instances directly through @atomically@.
-}
assertSTM :: IO w -> SystemT w STM Bool -> Property
assertSTM initW sys = monadicIO $ run (initW >>= runSystem (STM.atomically sys)) >>= assert

-- Components & world --------------------------------------------------------

newtype MapInt = MapInt Int deriving (Eq, Show, Arbitrary)
newtype UniqueInt = UniqueInt Int deriving (Eq, Show, Arbitrary)
newtype GlobalSum = GlobalSum (Sum Int) deriving (Eq, Show, Semigroup, Monoid, Arbitrary)

instance Component MapInt where type Storage MapInt = STM.TMap MapInt
instance Component UniqueInt where type Storage UniqueInt = STM.TUnique UniqueInt
instance Component GlobalSum where type Storage GlobalSum = STM.TGlobal GlobalSum

-- Generates @World@, @initWorld :: IO World@, and @EntityCounter@.
makeWorld "World" [''MapInt, ''UniqueInt, ''GlobalSum]

-- | An IO-only store: a cached 'Map'. It has no STM instances, so it can only
-- be touched from IO systems, never from inside an STM transaction.
newtype CachedInt = CachedInt Int deriving (Eq, Show, Arbitrary)
instance Component CachedInt where type Storage CachedInt = Cache 64 (Map CachedInt)

{- | A single world that mixes an IO-only cached 'Map' (@CachedInt@) with an
STM 'STM.TMap' (@MapInt@). The IO-backed 'STM.EntityCounter' added by
@makeWorld@ ties the two together: entities are allocated in IO but can be
populated either in IO (the cache) or inside a transaction (the TMap).

Generates @MixedWorld@ and @initMixedWorld :: IO MixedWorld@.
-}
makeWorld "MixedWorld" [''CachedInt, ''MapInt]

-- Map: set/get roundtrips ---------------------------------------------------

{- | Monad-polymorphic set/get body so the same logic runs over both the IO
and STM instance sets of the store.
-}
setGetBody
  :: forall w m c
   . (Get w m c, Set w m c, Destroy w m c, Eq c)
  => Proxy c
  -> [(Entity, c)]
  -> [Entity]
  -> Entity
  -> c
  -> [(Entity, c)]
  -> [Entity]
  -> SystemT w m Bool
setGetBody _ sets1 dels1 ety c sets2 dels2 = do
  forM_ sets1 $ uncurry set
  forM_ dels1 $ flip destroy (Proxy @c)
  set ety c
  forM_ (filter ((/= ety) . fst) sets2) $ uncurry set
  forM_ (filter (/= ety) dels2) $ flip destroy (Proxy @c)
  c' <- get ety
  return (c == c')

prop_setGetMapIO
  :: [(Entity, MapInt)] -> [Entity] -> Entity -> MapInt -> [(Entity, MapInt)] -> [Entity] -> Property
prop_setGetMapIO s1 d1 e c s2 d2 =
  assertSys initWorld (setGetBody (Proxy @MapInt) s1 d1 e c s2 d2)

prop_setGetMapSTM
  :: [(Entity, MapInt)] -> [Entity] -> Entity -> MapInt -> [(Entity, MapInt)] -> [Entity] -> Property
prop_setGetMapSTM s1 d1 e c s2 d2 =
  assertSTM initWorld (setGetBody (Proxy @MapInt) s1 d1 e c s2 d2)

-- Map: members and destroy --------------------------------------------------

prop_mapMembers :: [(Entity, MapInt)] -> [Entity] -> Property
prop_mapMembers sets dels = assertSys initWorld $ do
  forM_ sets $ uncurry set
  forM_ dels $ flip destroy (Proxy @MapInt)
  present <- cfold (\acc (_ :: MapInt, e :: Entity) -> e : acc) []
  -- The live set is everything we set, minus everything we deleted.
  let expected = nub [e | (e, _) <- sets, e `notElem` dels]
  pure $ sort present == sort expected

-- Unique: at most one live entity ------------------------------------------

prop_uniqueSingle :: NonEmptyList (Entity, UniqueInt) -> Property
prop_uniqueSingle (NonEmpty kvs) = assertSys initWorld $ do
  forM_ kvs $ uncurry set
  let (lastE, lastV) = last kvs
  ex <- exists lastE (Proxy @UniqueInt)
  v <- get lastE
  ms <- cfold (\acc (_ :: UniqueInt, e :: Entity) -> e : acc) []
  pure $ ex && v == lastV && length ms <= 1

prop_uniqueDestroy :: Entity -> UniqueInt -> Property
prop_uniqueDestroy e v = assertSys initWorld $ do
  set e v
  destroy e (Proxy @UniqueInt)
  not <$> exists e (Proxy @UniqueInt)

-- Global --------------------------------------------------------------------

prop_globalSetGet :: Entity -> GlobalSum -> Property
prop_globalSetGet e v = assertSys initWorld $ do
  set e v
  v' <- get e
  ex <- exists e (Proxy @GlobalSum)
  pure $ v == v' && ex

prop_globalDefault :: Property
prop_globalDefault = once $ assertSys initWorld $ do
  v <- get global
  pure $ v == (mempty :: GlobalSum)

-- Entity creation in STM ----------------------------------------------------

prop_newEntityFresh :: Property
prop_newEntityFresh = once $ assertSys initWorld $ do
  let n = 100
  es <- STM.atomically $ replicateM n (STM.newEntity (MapInt 0))
  let ids = [e | Entity e <- es]
  STM.EntityCounter (Sum c) <- get global
  pure $ length (nub ids) == n && c == n

-- check / retry -------------------------------------------------------------

prop_checkProceeds :: Entity -> MapInt -> Property
prop_checkProceeds e v = once $ assertSys initWorld $ do
  set e v
  r <- STM.atomically $ do
    ex <- exists e (Proxy @MapInt)
    STM.check ex
    get e
  pure $ r == v

{- | A consumer transaction blocks on @check@ (i.e. STM @retry@) until a
producer writes the component, then observes the written value.
-}
prop_retryProducerConsumer :: Property
prop_retryProducerConsumer = once $ monadicIO $ do
  res <- run $ do
    w <- initWorld
    let
      e = Entity 5
      target = MapInt 42
    result <- newEmptyMVar
    _ <- forkIO $ flip runSystem w $ do
      v <- STM.atomically $ do
        ex <- exists e (Proxy @MapInt)
        STM.check ex
        get e
      liftIO $ putMVar result v
    -- Let the consumer reach its retry, then produce.
    threadDelay 50000
    runSystem (STM.atomically (set e target)) w
    timeout 2000000 (takeMVar result)
  assert (res == Just (MapInt 42))

-- Concurrency ---------------------------------------------------------------

{- | Many threads concurrently allocate entities; the IO-backed EntityCounter
must hand out unique ids and the STM Map must hold them all.
-}
prop_concurrentNewEntity :: Property
prop_concurrentNewEntity = once $ monadicIO $ do
  let
    nThreads = 8
    perThread = 200
  (totalIds, uniqueIds, memberCount) <- run $ do
    w <- initWorld
    collected <- C.newTVarIO ([] :: [Int])
    dones <- replicateM nThreads newEmptyMVar
    flip runSystem w $ do
      forM_ dones $ \done -> STM.forkSys $ do
        forM_ [1 .. perThread] $ \_ -> do
          Entity e <- STM.atomically (STM.newEntity (MapInt 0))
          liftIO $ C.atomically $ C.modifyTVar' collected (e :)
        liftIO $ putMVar done ()
      liftIO $ mapM_ takeMVar dones
    ids <- C.atomically (C.readTVar collected)
    mc <- runSystem (cfold (\acc (_ :: MapInt, _ :: Entity) -> acc + 1) (0 :: Int)) w
    pure (length ids, length (nub ids), mc)
  assert $ totalIds == nThreads * perThread && uniqueIds == totalIds && memberCount == totalIds

-- | Concurrent atomic increments of a Global must not lose updates.
prop_concurrentGlobal :: Property
prop_concurrentGlobal = once $ monadicIO $ do
  let
    nThreads = 8
    perThread = 500
  final <- run $ do
    w <- initWorld
    dones <- replicateM nThreads newEmptyMVar
    flip runSystem w $ do
      forM_ dones $ \done -> STM.forkSys $ do
        forM_ [1 .. perThread] $ \_ ->
          STM.atomically $ modify global (\(GlobalSum s) -> GlobalSum (s + 1))
        liftIO $ putMVar done ()
      liftIO $ mapM_ takeMVar dones
    runSystem (get global) w
  let GlobalSum gs = final
  assert $ getSum gs == nThreads * perThread

-- Mixed IO/STM world --------------------------------------------------------

{- | Within one IO system, drive the cached 'Map' directly while routing
transactional reads and writes of the STM 'STM.TMap' through @atomically@.
Both stores are keyed by the same 'Entity', demonstrating that the two store
families coexist in a single world. -}
prop_mixedRoundtrip :: Entity -> CachedInt -> MapInt -> Property
prop_mixedRoundtrip e cv mv = assertSys initMixedWorld $ do
  -- Cached Map: plain IO access.
  set e cv
  -- STM TMap: writes must happen inside a transaction.
  STM.atomically $ set e mv
  cv' <- get e -- read back from the cached Map (IO)
  exC <- exists e (Proxy @CachedInt)
  (mv', exM) <- STM.atomically $ do -- read back from the TMap (STM)
    v <- get e
    ex <- exists e (Proxy @MapInt)
    pure (v, ex)
  pure $ cv == cv' && mv == mv' && exC && exM

{- | Many threads concurrently allocate entities and populate the STM 'TMap'
inside transactions; the IO-backed 'EntityCounter' keeps ids unique. Afterwards
the main thread, back in IO, decorates every spawned entity with a 'CachedInt'
in the cached 'Map' — a store the worker transactions could not have touched. -}
prop_mixedConcurrentSpawn :: Property
prop_mixedConcurrentSpawn = once $ monadicIO $ do
  let
    nThreads = 6
    perThread = 100
  (uniqueIds, mapCount, cacheCount) <- run $ do
    w <- initMixedWorld
    collected <- C.newTVarIO ([] :: [Int])
    dones <- replicateM nThreads newEmptyMVar
    flip runSystem w $ do
      forM_ dones $ \done -> STM.forkSys $ do
        forM_ [1 .. perThread] $ \_ -> do
          Entity e <- STM.atomically (STM.newEntity (MapInt 7))
          liftIO $ C.atomically $ C.modifyTVar' collected (e :)
        liftIO $ putMVar done ()
      liftIO $ mapM_ takeMVar dones
    ids <- C.atomically (C.readTVar collected)
    -- Back in IO: give each spawned entity a cached-Map component.
    flip runSystem w $ forM_ ids $ \e -> set (Entity e) (CachedInt e)
    mc <- runSystem (cfold (\acc (_ :: MapInt, _ :: Entity) -> acc + 1) (0 :: Int)) w
    cc <- runSystem (cfold (\acc (_ :: CachedInt, _ :: Entity) -> acc + 1) (0 :: Int)) w
    pure (length (nub ids), mc, cc)
  assert $
    uniqueIds == nThreads * perThread
      && mapCount == uniqueIds
      && cacheCount == uniqueIds

-- ---------------------------------------------------------------------------

return []
main :: IO Bool
main = $quickCheckAll
