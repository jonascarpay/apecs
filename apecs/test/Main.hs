{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -w #-}

import qualified Control.Exception           as E
import           Control.Monad
import qualified Data.Foldable               as F
import qualified Data.IntSet                 as S
import           Data.IORef
import           Data.List                   ((\\), delete, nub, sort)
import qualified Data.IntMap.Strict          as IM
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as Set
import qualified Data.Vector.Unboxed         as U
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Printf                 (printf)

import           Apecs
import           Apecs.Core
import           Apecs.Experimental.Children
import           Apecs.Experimental.Reactive
import           Apecs.Experimental.Stores
import           Apecs.Stores
import           Apecs.TH
import           Apecs.TH.Tags
import           Apecs.Util

type Vec = (Double, Double)

-- Preamble
instance Arbitrary Entity where
  arbitrary = Entity . getNonNegative <$> arbitrary

assertSys :: IO w -> System w Bool -> Property
assertSys initW sys = monadicIO $ run (initW >>= runSystem sys) >>= assert

genericSetGet :: forall w c.
  ( ExplGet IO (Storage c)
  , ExplSet IO (Storage c)
  , ExplDestroy IO (Storage c)
  , Has w IO c
  , Eq c
  , Arbitrary c )
  => IO w
  -> c
  -> [(Entity, c)] -> [Entity]
  -> Entity -> c
  -> [(Entity, c)] -> [Entity]
  -> Property
genericSetGet initSys _ sets1 dels1 ety c sets2 dels2 = do
  assertSys initSys $ do
    -- insert and delete random data
    forM_ sets1 $ uncurry set
    forM_ dels1 $ flip destroy (Proxy @c)
    set ety c
    forM_ (filter ((/= ety) . fst) sets2) $ uncurry set
    forM_ (filter (/= ety)         dels2) $ flip destroy (Proxy @c)
    c' <- get ety
    return (c == c')

genericSetSet :: forall w c.
  ( ExplGet IO (Storage c)
  , ExplSet IO (Storage c)
  , ExplDestroy IO (Storage c)
  , Has w IO c
  , Eq c
  , Arbitrary c )
  => IO w
  -> c
  -> [(Entity, c)] -> [Entity]
  -> Entity -> c
  -> [(Entity, c)] -> [Entity]
  -> c
  -> [(Entity, c)] -> [Entity]
  -> Property
genericSetSet initSys _ sets1 dels1 ety c1 sets2 dels2 c2 sets3 dels3 = do
  assertSys initSys $ do
    -- insert and delete random data
    forM_ sets1 $ uncurry set
    forM_ dels1 $ flip destroy (Proxy @c)
    set ety c1
    forM_ (filter ((/= ety) . fst) sets2) $ uncurry set
    forM_ (filter (/= ety)         dels2) $ flip destroy (Proxy @c)
    set ety c2
    forM_ (filter ((/= ety) . fst) sets3) $ uncurry set
    forM_ (filter (/= ety)         dels3) $ flip destroy (Proxy @c)
    c' <- get ety
    return (c2 == c')

-- Tests whether writing and reading gives back the original component
newtype MapInt = MapInt Int deriving (Eq, Show, Arbitrary)
instance Component MapInt where type Storage MapInt = Map MapInt
makeWorld "Simple" [''MapInt]

prop_setGetMap = genericSetGet initSimple (undefined :: MapInt)
prop_setSetMap = genericSetSet initSimple (undefined :: MapInt)

prop_destroyAll ety = assertSys initSimple $ do
  set ety (MapInt 1)
  destroy ety (Proxy @SimpleDestructible)
  not <$> exists ety (Proxy @MapInt)

-- Tests whether this is also true for caches
newtype CacheInt = CacheInt Int deriving (Eq, Show, Arbitrary)
instance Component CacheInt where type Storage CacheInt = Cache 2 (Map CacheInt)
makeWorld "Cached" [''CacheInt]

prop_setGetCache = genericSetGet initCached (undefined :: CacheInt)
prop_setSetCache = genericSetSet initCached (undefined :: CacheInt)

prop_cacheUnique :: [CacheInt] -> [Entity] -> [(Entity, CacheInt)] -> Property
prop_cacheUnique eInit eDel eSet = assertSys initCached $ do
  mapM newEntity eInit
  mapM (flip set (Not @CacheInt)) eDel
  mapM (uncurry set) eSet
  es <- cfold (\a (_ :: CacheInt, Entity e) -> e : a) []
  pure $ es == nub es

-- Tests basic tuple functionality
newtype T1 = T1 Int deriving (Eq, Show, Arbitrary)
newtype T2 = T2 Int deriving (Eq, Show, Arbitrary)
newtype T3 = T3 Int deriving (Eq, Show, Arbitrary)
instance Component T1 where type Storage T1 = Map T1
instance Component T2 where type Storage T2 = Map T2
instance Component T3 where type Storage T3 = Map T3

makeWorld "Tuples" [''T1, ''T2, ''T3]

newtype G1 = G1 () deriving (Eq, Show, Arbitrary, Semigroup, Monoid)
instance Component G1 where type Storage G1 = Global G1

-- Tests Enumerable class
makeWorld "WorldEnumerable" [''G1, ''T1, ''T2, ''T3]
makeTaggedComponents "WorldEnumerable" [''G1, ''T1, ''T2, ''T3]
-- Generate a (T1, T2, T3) tuple in a contrived way
-- (that allows processing component lists when placed in external file)
pure <$> makeInstanceFold mkTupleT "WorldEnumerableShowable" [''T1, ''T2, ''T3]

worldEntityIds :: System WorldEnumerable S.IntSet
worldEntityIds = do
  s :: Storage WorldEnumerableEnumerable <- getStore
  explMemberSet s

prop_enumerable :: [Entity] -> [(Entity, (T1, T2))] -> [(Entity, T3)] -> Property
prop_enumerable dels t12s t3s = assertSys initWorldEnumerable $ do
  forM_ t12s $ \(e, (t1, t2)) -> set e t1 >> set e t2
  forM_ t3s $ \(e, t3) -> set e t3

  let expectedBefore = S.fromList (map (unEntity . fst) t12s ++ map (unEntity . fst) t3s)
  actualBefore <- worldEntityIds

  everything <- forM (S.toList actualBefore) (get . Entity)
  let it = show @[Maybify WorldEnumerableShowable] everything
  guard (length it > 0)

  forM_ dels $ \e -> destroy e (Proxy @WorldEnumerableDestructible)

  let expectedAfter = expectedBefore `S.difference` S.fromList (map unEntity dels)
  actualAfter <- worldEntityIds
  return (expectedBefore == actualBefore && expectedAfter == actualAfter)

prop_tags_lookup :: [(Entity, (T1, T2))] -> [(Entity, T3)] -> Property
prop_tags_lookup t12s t3s = assertSys initWorldEnumerable $ do
  forM_ t12s $ \(e, (t1, t2)) -> set e t1 >> set e t2
  forM_ t3s $ \(e, t3) -> set e t3

  entities <- worldEntityIds

  eav <- fmap M.fromList . forM (map Entity $ S.toList entities) $ \e -> do
    tagged <- forM [minBound .. maxBound] $ \t -> fmap (t,) <$> lookupWorldEnumerableTag e t
    pure (e, M.fromList [ (t, v) | Just (t, v) <- tagged ])

  let it = show (eav :: M.Map Entity (M.Map WorldEnumerableTag WorldEnumerableSum))
  guard (length it > 0)

  pure True

prop_tags_get :: [(Entity, (T1, T2))] -> [(Entity, T3)] -> Property
prop_tags_get t12s t3s = assertSys initWorldEnumerable $ do
  forM_ t12s $ \(e, (t1, t2)) -> set e t1 >> set e t2
  forM_ t3s $ \(e, t3) -> set e t3

  -- arbitrary will produce overlapping entity sets for t12s and t3s
  -- the correct set of components for each entity is known at runtime
  let has_t12s = S.fromList (map (unEntity . fst) t12s)
  let has_t3s = S.fromList (map (unEntity . fst) t3s)

  forM_ (S.toList $ has_t12s <> has_t3s) $ \ety -> do
    let t12 = [[TT1, TT2] | ety `S.member` has_t12s]
    let t3 = [[TT3] | ety `S.member` has_t3s]
    let
      expected =
        -- XXX: matching the order is important.
        -- getWorldEnumerableTags will iterate in the "constructor order"
        -- derived from the filtered component type list.
        concat (t12 ++ t3)
    tags <- entityTags $ Entity ety
    unless (tags == expected) $ do
      error $ show (tags, expected)

  pure True

prop_count_components :: [(Entity, T1)] -> [(Entity, T2)] -> [(Entity, T3)] -> Property
prop_count_components t1s t2s t3s = assertSys initWorldEnumerable $ do
  forM_ t1s $ uncurry set
  forM_ t2s $ uncurry set
  forM_ t3s $ uncurry set

  counts <- countWorldEnumerableComponents
  let countMap = M.fromList counts

  let expectedT1 = length $ nub $ map fst t1s
  let expectedT2 = length $ nub $ map fst t2s
  let expectedT3 = length $ nub $ map fst t3s

  -- G1 is Global and should not appear in counts
  return $ M.lookup TT1 countMap == Just expectedT1
        && M.lookup TT2 countMap == Just expectedT2
        && M.lookup TT3 countMap == Just expectedT3
        && M.lookup TG1 countMap == Nothing

prop_count_combinations :: [(Entity, (T1, T2))] -> [(Entity, T3)] -> Property
prop_count_combinations t12s t3s = assertSys initWorldEnumerable $ do
  forM_ t12s $ \(e, (t1, t2)) -> set e t1 >> set e t2
  forM_ t3s $ \(e, t3) -> set e t3

  entities <- worldEntityIds
  combos <- countCombinations entities

  let has_t12s = S.fromList (map (unEntity . fst) t12s)
  let has_t3s = S.fromList (map (unEntity . fst) t3s)
  let entityTags ety =
        (if ety `S.member` has_t12s then [TT1, TT2] else [])
        ++ (if ety `S.member` has_t3s then [TT3] else [])
  let expected = M.fromListWith (+)
        [ (Set.fromList (entityTags ety), 1 :: Int)
        | ety <- S.toList (has_t12s <> has_t3s)
        ]

  return $ combos == expected

prop_setGetTuple = genericSetGet initTuples (undefined :: (T1,T2,T3))
prop_setSetTuple = genericSetSet initTuples (undefined :: (T1,T2,T3))

-- Tests Reactive store properties
newtype TestEnum = TestEnum Bool deriving (Eq, Show, Bounded, Enum, Arbitrary)
instance Component TestEnum where type Storage TestEnum = Reactive (EnumMap TestEnum) (Map TestEnum)

makeWorld "ReactiveWld" [''TestEnum]

prop_setGetReactive = genericSetGet initReactiveWld (undefined :: TestEnum)
prop_setSetReactive = genericSetSet initReactiveWld (undefined :: TestEnum)
prop_lookupValid :: [(Entity, TestEnum)] -> [Entity] -> Property
prop_lookupValid writes deletes = assertSys initReactiveWld $ do
  forM_ writes  $ uncurry set
  forM_ deletes $ flip destroy (Proxy @TestEnum)

  let getAll = cfold (flip (:)) [] :: SystemT ReactiveWld IO [(TestEnum, Entity)]
  et <- fmap snd . filter ((== TestEnum True ) . fst) <$> getAll
  ef <- fmap snd . filter ((== TestEnum False) . fst) <$> getAll

  rt <- withReactive $ enumLookup (TestEnum True)
  rf <- withReactive $ enumLookup (TestEnum False)

  return (  sort rt == sort et
         && sort rf == sort ef
         && all (`notElem` ef) et
         )

-- Tests Reactive component counting
newtype TestCount = TestCount Bool deriving (Eq, Show, Bounded, Enum, Arbitrary)
instance Component TestCount where type Storage TestCount = Reactive (ComponentCounter TestCount) (Map TestCount)

makeWorld "ReactiveCountWld" [''TestCount]

prop_setGetReactiveCount = genericSetGet initReactiveCountWld (undefined :: TestCount)
prop_setSetReactiveCount = genericSetSet initReactiveCountWld (undefined :: TestCount)
prop_reactiveCounts :: [(Entity, TestCount)] -> [Entity] -> Property
prop_reactiveCounts writes deletes = assertSys initReactiveCountWld $ do
  forM_ writes  $ uncurry set
  forM_ deletes $ flip destroy (Proxy @TestCount)

  count <- withReactive $ readComponentCount @TestCount

  return $ count == ComponentCount
    { componentCountCurrent = length existingEnts
    , componentCountMax = length writeEnts
    }
  where
  existingEnts = writeEnts \\ deleteEnts
  writeEnts = nub $ sort $ fst <$> writes
  deleteEnts = nub $ sort deletes

-- Tests Pushdown
newtype StackInt = StackInt Int deriving (Eq, Show, Arbitrary)
instance Component StackInt where type Storage StackInt = Pushdown Map StackInt

makeWorld "StackWld" [''StackInt]

prop_setGetStack = genericSetSet initStackWld (undefined :: StackInt)

-- Tests Child
type ChildT2 = Child T2
makeWorld "ChildTest" [''T1, ''ChildT2]

prop_setGetChild = genericSetGet initChildTest (undefined :: (T1, Child T2))
prop_setSetChild = genericSetSet initChildTest (undefined :: (T1, Child T2))
-- | This instance is only for the generic tests. It hard-codes each generated
-- @Child T2@ component value with the global entity as the parent.
instance Arbitrary (Child T2) where
  arbitrary = Child <$> pure global <*> arbitrary

data ChildrenEx = ChildrenEx String deriving (Show)
instance E.Exception ChildrenEx
prop_children :: NonEmptyList (T1, NonEmptyList T2) -> Property
prop_children (NonEmpty writes) = assertSys initChildTest $ do
  forM_ writes $ \(t1, NonEmpty t2s) -> do
    -- Create a parent entity with the T1 component value.
    parent <- newEntity t1
    -- Create child entities with the T2 component values.
    children <- fmap mconcat $ forM t2s $ \t2 -> do
      child <- newEntity $ Child parent t2
      pure [child]
    -- For each child entity, check that we can fetch it, its parent is
    -- correct, and its component value is good.
    forM_ children $ \child -> do
      Child p t2 :: Child T2 <- get child
      unless (p == parent) $ do
        liftIO $ E.throwIO $ ChildrenEx $
          printf "Child entity %d's parent of %d does not match set parent of %d"
            (unEntity child)
            (unEntity p)
            (unEntity parent)
      unless (t2 `elem` t2s) $ do
        liftIO $ E.throwIO $ ChildrenEx $
          printf
            "Child entity %d's component value of %s is not present in the input %s"
            (unEntity child)
            (show t2)
            (show t2s)
    -- Fetch the child entity list from the parent entity and check its validity.
    ChildList children' :: ChildList T2 <- get parent
    unless (sort children == sort (F.toList children')) $ do
      liftIO $ E.throwIO $ ChildrenEx $
        printf
          "Mismatch between fetched child list (%s) and created child entities (%s)"
          (show $ sort $ F.toList children')
          (show $ sort children)
    -- Reparent the first child entity in this group to be under the global entity.
    let child1 = head children
    modify child1 $ \(ChildValue t2) -> Child @T2 global t2
    -- Check that the first child entity's parent was actually updated.
    Child child1Parent child1T2 :: Child T2 <- get child1
    unless (child1Parent == global) $ do
      liftIO $ E.throwIO $ ChildrenEx $
        printf
          "Reparented child entity %d should have been under global entity but is under %d"
          (unEntity child1)
          (unEntity child1Parent)
    -- Check that the original parent no longer sees the reparented child as
    -- its own child.
    get parent >>= \case
      Nothing -> pure () -- Parent only had 1 child, and this child just reparented.
      Just (ChildList children'' :: ChildList T2) -> do
        unless (sort (delete child1 children) == sort (F.toList children'')) $ do
          liftIO $ E.throwIO $ ChildrenEx $
            printf
              "Mismatch between fetched child list (%s) and modified child entities (%s)"
              (show $ sort $ F.toList children'')
              (show $ sort children)

  -- Check that the global entity's children have component values aligning
  -- with the first T2 value in each group of the input list, as the first
  -- child of each group was previously reparented to be under the global
  -- entity.
  ChildList children :: ChildList T2 <- get global
  forM_ (zip (sort $ F.toList children) $ fmap (head . getNonEmpty . snd) writes) $ \(child, expT2) -> do
    ChildValue t2 :: ChildValue T2 <- get child
    unless (t2 == expT2) $ do
      liftIO $ E.throwIO $ ChildrenEx $
        "Child component value mismatch within those entities reparented under the global entity"

  -- Check that a cascading destroy works.
  destroy global $ Proxy @(ChildList T2)
  get global >>= \case
    Nothing -> pure () -- Expected case - there's no child list as they were all just destroyed.
    Just (ChildList children' :: ChildList T2) -> do
      liftIO $ E.throwIO $ ChildrenEx $
        printf "Left over child entities (%s) after cascade destroy on the global entity"
          (show $ F.toList children')

  return True

return []
main = $quickCheckAll
