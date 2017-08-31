{-# LANGUAGE FlexibleContexts #-}

module Control.ECS (
  module Control.ECS.Core,

  -- Reader
  asks, ask, liftIO, lift,

  -- Self
  newEntityWith, EntityCounter, nextEntity,
  runGC,
) where

import Prelude hiding (read, all)
import Control.Monad.Reader

import Control.ECS.Core
import Control.ECS.Storage.Mutable
import Control.ECS.Storage.Tuples ()
import System.Mem (performMajorGC)

newtype EntityCounter = ECount Int deriving (Eq, Show, Num)
instance Component EntityCounter where
  type Storage EntityCounter = Global EntityCounter

instance Monoid EntityCounter where
  mempty = 0
  mappend = (+)

{-# INLINE nextEntity #-}
nextEntity :: Has w EntityCounter => System w (Entity a)
nextEntity = do Reads (ECount c) :: Reads EntityCounter <- read (-1)
                let w :: Writes EntityCounter = Writes (ECount (c+1))
                write w (-1)
                return (Entity c)

{-# INLINE newEntityWith #-}
newEntityWith :: (Has w c, Has w EntityCounter) => Elem c -> System w (Entity a)
newEntityWith c = do e <- nextEntity
                     writeRaw c e
                     return e

runGC :: System w ()
runGC = liftIO performMajorGC
