{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -w #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as U
import Data.IORef
import Data.List (sort)
import Control.Monad

import Apecs
import Apecs.Reactive
import Apecs.Core
import Apecs.Stores
import Apecs.Util

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
  -> [(Entity, c)]
  -> [Entity]
  -> [(Entity, c)]
  -> Entity -> c
  -> Property
genericSetGet initSys _ sets1 dels sets2 ety c = do
  assertSys initSys $ do
    -- insert and delete random data
    forM_ sets1 $ uncurry set
    forM_ dels $ flip destroy (Proxy @c)
    forM_ sets2 $ uncurry set
    set ety c
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
  -> [(Entity, c)]
  -> [Entity]
  -> [(Entity, c)]
  -> Entity -> c -> c
  -> Property
genericSetSet initSys _ sets1 dels sets2 ety c1 c2 = do
  assertSys initSys $ do
    -- insert and delete random data
    forM_ sets1 $ uncurry set
    forM_ dels $ flip destroy (Proxy @c)
    forM_ sets2 $ uncurry set
    set ety c1
    set ety c2
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

newtype TestBool = TestBool Bool deriving (Eq, Show, Bounded, Enum, Arbitrary)
instance Component TestBool where type Storage TestBool = Reactive (EnumMap TestBool) (Map TestBool)

makeWorld "ReactiveWld" [''TestBool]

prop_setGetReactive = genericSetGet initReactiveWld (undefined :: TestBool)
prop_setSetReactive = genericSetSet initReactiveWld (undefined :: TestBool)
prop_lookupValid :: [(Entity, TestBool)] -> [Entity] -> Property
prop_lookupValid writes deletes = assertSys initReactiveWld $ do
  forM_ writes  $ uncurry set
  forM_ deletes $ flip destroy (Proxy @TestBool)

  et <- fmap snd . filter ((== TestBool True ) . fst) <$> getAll
  ef <- fmap snd . filter ((== TestBool False) . fst) <$> getAll

  let lookup enum = rget >>= flip mapLookup enum
  rt <- lookup (TestBool True)
  rf <- lookup (TestBool False)

  return (  sort rt == sort et
         && sort rf == sort ef
         && all (`notElem` ef) et
         )

return []
main = $quickCheckAll
