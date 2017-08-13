{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State

import Control.ECS.Core

newtype SimpleMap c = SimpleMap {unSimpleMap :: M.IntMap c} deriving (Eq, Show, Monoid)

instance SStorage (SimpleMap c) where

  type SRuntime (SimpleMap c) = Maybe c

  sEmpty = return mempty

  sSlice = M.keysSet . unSimpleMap <$> get
  sRetrieve (Entity e) = M.lookup e . unSimpleMap <$> get

  sStore (Entity e) (Just x) = modify $ \(SimpleMap s) -> SimpleMap (M.insert e x s)
  sStore (Entity e) Nothing  = modify $ \(SimpleMap s) -> SimpleMap (M.delete e s)


newtype SimpleFlag = SimpleFlag {unSimpleFlag :: S.IntSet} deriving (Eq, Show, Monoid)

instance SStorage SimpleFlag where

  type SRuntime SimpleFlag = Bool

  sEmpty = return mempty

  sSlice = unSimpleFlag <$> get
  sRetrieve (Entity e) = S.member e . unSimpleFlag <$> get

  sStore (Entity e) True  = modify $ \(SimpleFlag s) -> SimpleFlag (S.insert e s)
  sStore (Entity e) False = modify $ \(SimpleFlag s) -> SimpleFlag (S.delete e s)


newtype Global c = Global { unGlobal :: c } deriving (Monoid, Eq, Show)

instance Monoid c => SStorage (Global c) where
  type SRuntime (Global c) = Global c
  sEmpty      = return mempty
  sSlice      = return . S.singleton $ -1
  sRetrieve _ = get
  sStore _ c  = put c

  type SRuntime EntityCounter = Int

  sEmpty = return (EntityCounter 0)

  sSlice = return . S.singleton $ -1
  sRetrieve _ = getCount <$> get
  sStore _ count = put (EntityCounter count)
