{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import qualified Data.IntSet as S

import Apecs
import Apecs.Util
import qualified Apecs.Stores as S

type Vec = (Double, Double)

newtype Position = Position Vec deriving (Arbitrary, Eq, Show)
instance Component Position where
  type Storage Position = S.Logger (IndexLogger Position) (S.Map Position)

newtype CachePos = CachePos Vec deriving (Arbitrary, Eq, Show)
instance Component CachePos where
  type Storage CachePos = S.Map CachePos


newtype Velocity = Velocity Vec deriving (Arbitrary, Eq, Show)
instance Component Velocity where
  type Storage Velocity = S.Map Velocity


data Flag = Flag
instance Arbitrary Flag where arbitrary = return Flag
instance S.Flag Flag where flag = Flag
instance Component Flag where
  type Storage Flag = S.Set Flag

newtype IndexLogger c = IndexLogger S.IntSet
instance S.Log (IndexLogger c) where
  type LogComponent (IndexLogger c) = c
  logEmpty = IndexLogger mempty
  logOnSet (Entity e) _ _ (IndexLogger s) = IndexLogger $ S.insert e s
  logOnDestroy (Entity e) _ (IndexLogger s) = IndexLogger $ S.delete e s

newtype RandomEntity a = RandomEntity (Entity a) deriving (Eq, Show)
instance Arbitrary (RandomEntity a) where
  arbitrary = RandomEntity . Entity <$> arbitrary

newtype W1 c = W1 {getIdentity :: (Storage c)}
instance Component c => Has (W1 c) c where getStore = System $ asks getIdentity

data W2 a b = W2 { c1 :: Storage a
                 , c2 :: Storage b
                 }

instance (Component a, Component b) => Has (W2 a b) a where getStore = System $ asks c1
instance (Component a, Component b) => Has (W2 a b) b where getStore = System $ asks c2

getSetPos :: [(RandomEntity Position, Position)] -> RandomEntity Position -> Position -> Property
getSetPos cs (RandomEntity e) p = monadicIO $ run f >>= assert
  where
    f = do
      w :: Storage Position <- initStore
      runWith (W1 w) $ do
        forM_ cs $ \(RandomEntity ety, pos) -> set ety pos
        set e p
        Safe r <- get e
        return (r == Just p)

getSetVCPos :: [(RandomEntity (Velocity, CachePos), (Velocity, CachePos))] -> RandomEntity (Velocity, CachePos) -> (Velocity, CachePos) -> Property
getSetVCPos cs (RandomEntity e) (v,p) = monadicIO $ run f >>= assert
  where
    f = do
      wp :: Storage CachePos <- initStore
      wv :: Storage Velocity <- initStore
      runWith (W2 wp wv) $ do
        forM_ cs $ \(RandomEntity ety, pos) -> set ety pos
        set e (v,p)
        Safe r <- get e
        return (r == (Just v, Just p))

cmapVP :: [(RandomEntity (Velocity, CachePos), (Velocity, CachePos))] -> RandomEntity (Velocity, CachePos) -> (Velocity, CachePos) -> Property
cmapVP cs (RandomEntity e) (v,p) = monadicIO $ run f >>= assert
  where
    f = do
      let swapP (CachePos (x,y)) = CachePos (y,x)
          swapV (Velocity (x,y)) = Velocity (y,x)
      wp :: Storage CachePos <- initStore
      wv :: Storage Velocity <- initStore
      runWith (W2 wp wv) $ do
        forM_ cs $ \(RandomEntity ety, pos) -> set ety pos
        set e (v,p)
        cmap $ \(v,p) -> (swapV v, swapP p)
        Safe r <- get e
        return (r == (Just $ swapV v, Just $ swapP p))

main = do
  quickCheck getSetPos
  quickCheck getSetVCPos
  quickCheck cmapVP
