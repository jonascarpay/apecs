{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeOperators, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State

import Control.ECS.Core

newtype SimpleMap c = SimpleMap c deriving (Eq, Show)

instance Component (SimpleMap c) where

  type Runtime (SimpleMap c) = Maybe c
  type Storage (SimpleMap c) = M.IntMap c
  type Env (SimpleMap c) m = Monad m

  empty = return $ Store mempty

  slice = Slice . M.keysSet . unStore <$> get
  retrieve (Entity e) = Reads . M.lookup e . unStore <$> get

  store (Entity e) (Writes (Just x)) = modify $ \(Store s) -> Store (M.insert e x s)
  store (Entity e) (Writes Nothing)  = modify $ \(Store s) -> Store (M.delete e s)


data SimpleFlag

instance Component SimpleFlag where

  type Runtime SimpleFlag = Bool
  type Storage SimpleFlag = S.IntSet
  type Env SimpleFlag m = Monad m

  empty = return $ Store mempty

  slice = Slice . unStore <$> get
  retrieve (Entity e) = Reads . S.member e . unStore <$> get

  store (Entity e) (Writes True)  = modify $ \(Store s) -> Store (S.insert e s)
  store (Entity e) (Writes False) = modify $ \(Store s) -> Store (S.delete e s)


data EntityCounter

instance Component EntityCounter where

  type Runtime EntityCounter = Int
  type Storage EntityCounter = Int
  type Env EntityCounter m = Monad m

  empty = return $ Store 0

  slice = return . Slice . S.singleton $ -1
  retrieve _ = Reads . unStore <$> get
  store _ (Writes count) = put (Store count)

createEntity :: forall w m. (Monad m, Has w EntityCounter) => System w m Entity
createEntity = embed $ do Reads c :: Reads EntityCounter <- retrieve undefined
                          store undefined (Writes (c+1))
                          return (Entity c)
