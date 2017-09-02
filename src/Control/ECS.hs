{-# LANGUAGE FlexibleContexts #-}

module Control.ECS (
  module Control.ECS.Core,

  -- Reader
  asks, ask, liftIO, lift,

  -- Self
  newEntity, newEntityFast, EntityCounter, nextEntity,
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
nextEntity = do Reads (ECount c) :: Reads EntityCounter <- readGlobal
                let w :: Writes EntityCounter = Writes (ECount (c+1))
                writeGlobal w
                return (Entity c)

{-# INLINE newEntity #-}
newEntity :: (Has w c, Has w EntityCounter) => Writes c -> System w (Entity c)
newEntity c = do e <- nextEntity
                 write c e
                 return e

{-# INLINE newEntityFast #-}
newEntityFast :: (Has w c, Has w EntityCounter) => Elem c -> System w (Entity c)
newEntityFast c = do e <- nextEntity
                     writeRaw c e
                     return e

runGC :: System w ()
runGC = liftIO performMajorGC
