{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State

import Control.ECS.Core

newtype SimpleMap c = SimpleMap {unSimpleMap :: M.IntMap c} deriving (Eq, Show, Monoid)

instance SStorage (SimpleMap c) where

  type SRuntime (SimpleMap c) = Maybe c

  sEmpty               = return mempty
  sSlice               = fmap Entity . M.keys . unSimpleMap <$> get
  sRetrieve (Entity e) = M.lookup e . unSimpleMap <$> get
  sMember (Entity e)   = M.member e . unSimpleMap <$> get
  sDestroy (Entity e)  = modify $ \(SimpleMap s) -> SimpleMap (M.delete e s)

  sStore (Entity e) (Just x) = modify $ \(SimpleMap s) -> SimpleMap (M.insert e x s)
  sStore e Nothing = sDestroy e


newtype SimpleFlag = SimpleFlag {unSimpleFlag :: S.IntSet} deriving (Eq, Show, Monoid)

instance SStorage SimpleFlag where

  type SRuntime SimpleFlag = Bool

  sEmpty               = return mempty
  sSlice               = fmap Entity . S.toList . unSimpleFlag <$> get
  sRetrieve (Entity e) = S.member e . unSimpleFlag <$> get
  sDestroy  (Entity e) = modify $ \(SimpleFlag s) -> SimpleFlag (S.delete e s)
  sMember              = sRetrieve

  sStore (Entity e) True  = modify $ \(SimpleFlag s) -> SimpleFlag (S.insert e s)
  sStore e False = sDestroy e


newtype Global c = Global { unGlobal :: c } deriving (Monoid, Eq, Show)
instance Monoid c => Component (Global c) where
  type Storage (Global c) = Global c

instance Monoid c => SStorage (Global c) where
  type SRuntime (Global c) = c
  sEmpty      = return mempty
  sSlice      = return [nullEntity]
  sRetrieve _ = unGlobal <$> get
  sDestroy _  = return ()
  sMember _   = return False
  sStore _ c  = put (Global c)

newtype EntityCounter = EntityCounter { unEntityCounter :: Int} deriving (Eq, Show)
instance Monoid EntityCounter where
  mempty = EntityCounter 0
  mappend (EntityCounter a) (EntityCounter b) = EntityCounter (a+b)

instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter
