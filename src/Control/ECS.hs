module Control.ECS (
  module Control.ECS.Core,

  -- Storage
  Component, Storage,

  -- Mutable
  HashTable, Global,

  -- Reader
  asks,

  -- Self
  EntityCounter,
) where

import Control.Monad.Reader

import Control.ECS.Core
import Control.ECS.Storage
import Control.ECS.Mutable

newtype EntityCounter = EntityCounter Int
instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

instance Monoid EntityCounter where
  mempty = EntityCounter 0
  EntityCounter e1 `mappend` EntityCounter e2 = EntityCounter (e1 + e2)
