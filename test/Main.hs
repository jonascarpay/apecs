{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Apecs
import Apecs.Types
import Apecs.TH
import Apecs.Stores
import Apecs.Logs

type Vec = (Double, Double)

-- Preamble
newtype RandomEntity a = RandomEntity {getRandom :: Entity a} deriving (Eq, Show)
instance Arbitrary (RandomEntity a) where
  arbitrary = RandomEntity . Entity . abs <$> arbitrary

assertSys :: IO w -> System w Bool -> Property
assertSys initW sys = monadicIO $ run (initW >>= runSystem sys) >>= assert

type Inserts a = [a]
type Deletes a = [RandomEntity a]
type Writes a  = [(RandomEntity a, a)]
type Scramble a = (Inserts a, Writes a, Deletes a)

insertAll :: (Has w EntityCounter, Has w c) => Inserts c -> System w ()
insertAll = mapM_ newEntity
writeAll  :: Has w c => Writes c -> System w ()
writeAll = mapM_ $ \(e, w) -> set (getRandom e) w
deleteAll :: Has w c => Deletes c -> System w ()
deleteAll = mapM_ (destroy . getRandom)
scramble :: (Has w EntityCounter, Has w c) => Scramble c -> System w ()
scramble (is, ws, ds) = insertAll is >> writeAll ws >> deleteAll ds

-- Tests whether writing and reading gives back the original component
newtype MapInt = MapInt Int deriving (Eq, Show, Arbitrary)
instance Component MapInt where type Storage MapInt = Map MapInt
makeWorld "SetGetMI" [''MapInt]

setGetProp :: Scramble MapInt -> RandomEntity MapInt -> MapInt -> Property
setGetProp scr (RandomEntity re) rw = assertSys initSetGetMI $ do
  scramble scr
  set re rw
  Safe r :: Safe MapInt <- get re
  return (r == Just rw)

-- Tests whether this is also true for caches
newtype CacheInt = CacheInt Int deriving (Eq, Show, Arbitrary)
instance Component CacheInt where type Storage CacheInt = Cache 2 (Map CacheInt)
makeWorld "SetGetCI" [''CacheInt]

setGetPropC :: Scramble CacheInt -> RandomEntity CacheInt -> CacheInt -> Property
setGetPropC scr (RandomEntity re) rw = assertSys initSetGetCI $ do
  scramble scr
  set re rw
  Safe r :: Safe CacheInt <- get re
  return (r == Just rw)

-- Tests basic tuple functionality
newtype T1 = T1 Int deriving (Eq, Show, Arbitrary)
newtype T2 = T2 Int deriving (Eq, Show, Arbitrary)
newtype T3 = T3 Int deriving (Eq, Show, Arbitrary)
instance Component T1 where type Storage T1 = Map T1
instance Component T2 where type Storage T2 = Map T2
instance Component T3 where type Storage T3 = Map T3

makeWorld "Tuples" [''T1, ''T2, ''T3]

setGetTuple :: (T1, T2, T3) -> Inserts (T1, T2, T3) -> Property
setGetTuple w@(T1 n1, T2 n2, T3 n3) ws = assertSys initTuples $ do
  e <- newEntity w
  insertAll ws
  cmap $ \(T1 n) -> T1 (n+1)
  Safe (r1, r2, r3) <- get e
  return $ r1 == Just (T1 $ n1+1) && r2 == Just (T2 n2) && r3 == Just (T3 n3)

-- This Log should be able to track the members of the underlying store
newtype Members c = Members S.IntSet
instance PureLog Members c where
  pureEmpty = Members mempty
  pureOnSet     (Entity e) _ _ (Members s) = Members $ S.insert e s
  pureOnDestroy (Entity e) _   (Members s) = Members $ S.delete e s

data Logged = Logged deriving (Eq, Show)
instance Arbitrary Logged where arbitrary = return Logged
instance Component Logged where type Storage Logged = Logger (FromPure Members) (Map Logged)

makeWorld "LoggerProp" [''Logged]

loggerProp :: Scramble Logged -> Property
loggerProp s = assertSys initLoggerProp $ do
  scramble s
  Slice sl :: Slice Logged <- owners
  FromPure ref :: FromPure Members Logged <- getLog
  Members set <- liftIO$ readIORef ref
  return (sl == U.fromList (S.toList set))


main = do
  quickCheck setGetProp
  quickCheck setGetTuple
  quickCheck setGetPropC
  quickCheck loggerProp
