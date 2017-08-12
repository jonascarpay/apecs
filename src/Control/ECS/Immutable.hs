{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State
import Control.Lens

import Control.ECS.Types

newtype SimpleMap c = SimpleMap c deriving (Eq, Show)

instance Representable (SimpleMap c) where
  type Runtime (SimpleMap c) = Maybe c
  type Storage (SimpleMap c) = M.IntMap c

instance Monad m => Component m (SimpleMap c) where

  empty = return $ Store mempty

  slice = Slice . M.keysSet . _unStore <$> get
  retrieve (Entity e) = Reads . M.lookup e . _unStore <$> get

  store (Entity e) (Writes (Just x)) = unStore %= M.insert e x
  store (Entity e) (Writes Nothing)  = unStore %= M.delete e


data SimpleFlag

instance Representable SimpleFlag where
  type Runtime SimpleFlag = Bool
  type Storage SimpleFlag = S.IntSet

instance Monad m => Component m SimpleFlag where

  empty = return $ Store mempty

  slice = Slice . _unStore <$> get
  retrieve (Entity e) = Reads . S.member e . _unStore <$> get

  store (Entity e) (Writes True) = unStore %= S.insert e
  store (Entity e) (Writes True) = unStore %= S.delete e


data EntityCounter

instance Representable EntityCounter where
  type Runtime EntityCounter = Int
  type Storage EntityCounter = Int

instance Monad m => Component m EntityCounter where

  empty = return $ Store 0

  slice = return . Slice . S.singleton $ -1
  retrieve _ = Reads . _unStore <$> get
  store _ (Writes count) = unStore .= count
