{-# LANGUAGE FlexibleContexts #-}

module Control.ECS (
  module Control.ECS.Core,

  -- Mutable
  HashTable, Global, Cached, newCacheWith,

  -- Immutable
  Map, FlagSet,

  -- Reader
  asks, ask, liftIO, lift,

  -- Self
  newEntityWith, EntityCounter, nextEntity,
  runGC,
) where

import Control.Monad.Reader

import Control.ECS.Core
import Control.ECS.Storage.Mutable
import Control.ECS.Storage.Tuples ()
import Control.ECS.Storage.Immutable
import System.Mem (performMajorGC)

newtype EntityCounter = ECount Int deriving (Eq, Show, Num)
instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

instance Monoid EntityCounter where
  mempty = 0
  mappend = (+)

{-# INLINE nextEntity #-}
nextEntity :: Valid w EntityCounter => System w (Entity a)
nextEntity = do Reads (ECount c) :: Reads EntityCounter <- retrieve (-1)
                let w :: Writes EntityCounter = Writes (ECount (c+1))
                store w (-1)
                return (Entity c)

{-# INLINE newEntityWith #-}
newEntityWith :: (Valid w c, Valid w EntityCounter) => Writes c -> System w (Entity a)
newEntityWith c = do e <- nextEntity
                     store c e
                     return e

runGC :: System w ()
runGC = liftIO performMajorGC
