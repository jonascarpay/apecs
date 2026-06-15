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

import Apecs (cfold, destroy, exists, get, global, modify, runSystem, set)
import Apecs.Core
import Apecs.STM (STM)
import qualified Apecs.STM as STM

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
newtype TInt = TInt Int deriving (Eq, Show, Arbitrary)
newtype ShardInt = ShardInt Int deriving (Eq, Show, Arbitrary)
newtype UniqueInt = UniqueInt Int deriving (Eq, Show, Arbitrary)
newtype GlobalSum = GlobalSum (Sum Int) deriving (Eq, Show, Semigroup, Monoid, Arbitrary)

instance Component MapInt where type Storage MapInt = STM.Map MapInt
instance Component TInt where type Storage TInt = STM.TMap TInt
instance Component ShardInt where type Storage ShardInt = STM.Sharded 16 (STM.TMap ShardInt)
instance Component UniqueInt where type Storage UniqueInt = STM.Unique UniqueInt
instance Component GlobalSum where type Storage GlobalSum = STM.Global GlobalSum

-- Generates @World@, @initWorld :: IO World@, and (STM) @EntityCounter@.
STM.makeWorld "World" [''MapInt, ''TInt, ''ShardInt, ''UniqueInt, ''GlobalSum]

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

{- | Monad-polymorphic body shared by the per-store members properties:
@explMembers@ (via 'cfold') must report exactly the entities we set and did not
later destroy.
-}
membersBody
  :: forall w m c
   . (Get w m c, Set w m c, Destroy w m c, Members w m c)
  => Proxy c
  -> [(Entity, c)]
  -> [Entity]
  -> SystemT w m Bool
membersBody _ sets dels = do
  forM_ sets $ uncurry set
  forM_ dels $ flip destroy (Proxy @c)
  present <- cfold (\acc (_ :: c, e :: Entity) -> e : acc) []
  -- The live set is everything we set, minus everything we deleted.
  let expected = nub [e | (e, _) <- sets, e `notElem` dels]
  pure $ sort present == sort expected

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
prop_mapMembers sets dels = assertSys initWorld (membersBody (Proxy @MapInt) sets dels)

-- TMap: same contract as Map, exercised over both instance sets ------------

prop_setGetTMapIO
  :: [(Entity, TInt)] -> [Entity] -> Entity -> TInt -> [(Entity, TInt)] -> [Entity] -> Property
prop_setGetTMapIO s1 d1 e c s2 d2 =
  assertSys initWorld (setGetBody (Proxy @TInt) s1 d1 e c s2 d2)

prop_setGetTMapSTM
  :: [(Entity, TInt)] -> [Entity] -> Entity -> TInt -> [(Entity, TInt)] -> [Entity] -> Property
prop_setGetTMapSTM s1 d1 e c s2 d2 =
  assertSTM initWorld (setGetBody (Proxy @TInt) s1 d1 e c s2 d2)

prop_tmapMembers :: [(Entity, TInt)] -> [Entity] -> Property
prop_tmapMembers sets dels = assertSys initWorld (membersBody (Proxy @TInt) sets dels)

-- Sharded (TMap): same contract, with operations routed across shards -------

prop_setGetShardedIO
  :: [(Entity, ShardInt)] -> [Entity] -> Entity -> ShardInt -> [(Entity, ShardInt)] -> [Entity] -> Property
prop_setGetShardedIO s1 d1 e c s2 d2 =
  assertSys initWorld (setGetBody (Proxy @ShardInt) s1 d1 e c s2 d2)

prop_setGetShardedSTM
  :: [(Entity, ShardInt)] -> [Entity] -> Entity -> ShardInt -> [(Entity, ShardInt)] -> [Entity] -> Property
prop_setGetShardedSTM s1 d1 e c s2 d2 =
  assertSTM initWorld (setGetBody (Proxy @ShardInt) s1 d1 e c s2 d2)

-- | @explMembers@ must return every live entity exactly once, across shards.
prop_shardedMembers :: [(Entity, ShardInt)] -> [Entity] -> Property
prop_shardedMembers sets dels = assertSys initWorld (membersBody (Proxy @ShardInt) sets dels)

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

-- ---------------------------------------------------------------------------

return []
main :: IO Bool
main = $quickCheckAll
