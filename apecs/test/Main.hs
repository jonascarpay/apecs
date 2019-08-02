{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -w #-}

import           Control.Monad
import qualified Data.IntSet                 as S
import           Data.IORef
import           Data.List                   (sort)
import qualified Data.Vector.Unboxed         as U
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.List (nub)

import           Apecs
import           Apecs.Core
import           Apecs.Experimental.Reactive
import           Apecs.Experimental.Stores
import           Apecs.Stores
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

-- Tests Pushdown
newtype StackInt = StackInt Int deriving (Eq, Show, Arbitrary)
instance Component StackInt where type Storage StackInt = Pushdown Map StackInt

makeWorld "StackWld" [''StackInt]

prop_setGetStack = genericSetSet initStackWld (undefined :: StackInt)

return []
main = $quickCheckAll
