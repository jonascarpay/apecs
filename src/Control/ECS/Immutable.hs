{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State

import Control.ECS.Core

newtype SimpleMap c = SimpleMap {unSimpleMap :: M.IntMap c} deriving (Eq, Show, Monoid)

instance CStorage (SimpleMap c) where

  type Runtime (SimpleMap c) = Maybe c
  type Env (SimpleMap c) m = Monad m

  empty = return mempty

  slice = Slice . M.keysSet . unSimpleMap <$> get
  retrieve (Entity e) = Reads . M.lookup e . unSimpleMap <$> get

  store (Entity e) (Writes (Just x)) = modify $ \(SimpleMap s) -> SimpleMap (M.insert e x s)
  store (Entity e) (Writes Nothing)  = modify $ \(SimpleMap s) -> SimpleMap (M.delete e s)


newtype SimpleFlag = SimpleFlag {unSimpleFlag :: S.IntSet} deriving (Eq, Show, Monoid)

instance CStorage SimpleFlag where

  type Runtime SimpleFlag = Bool
  type Env SimpleFlag m = Monad m

  empty = return mempty

  slice = Slice . unSimpleFlag <$> get
  retrieve (Entity e) = Reads . S.member e . unSimpleFlag <$> get

  store (Entity e) (Writes True)  = modify $ \(SimpleFlag s) -> SimpleFlag (S.insert e s)
  store (Entity e) (Writes False) = modify $ \(SimpleFlag s) -> SimpleFlag (S.delete e s)


newtype EntityCounter = EntityCounter {getCount :: Int} deriving (Eq, Show)

instance CStorage EntityCounter where

  type Runtime EntityCounter = Int
  type Env EntityCounter m = Monad m

  empty = return (EntityCounter 0)

  slice = return . Slice . S.singleton $ -1
  retrieve _ = Reads . getCount <$> get
  store _ (Writes count) = put (EntityCounter count)

{-createEntity :: forall w m. (Monad m, Has w EntityCounter) => System w m Entity-}
{-createEntity = embed $ do Reads c :: Reads EntityCounter <- retrieve undefined-}
                          {-store undefined (Writes (c+1))-}
                          {-return (Entity c)-}
