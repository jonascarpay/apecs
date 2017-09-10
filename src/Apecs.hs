{-# LANGUAGE FlexibleContexts #-}

module Apecs (
  -- Core
  initStore,
  destroy, exists, sliceAll,
  get, set, setMaybe, modify,
  A.map, rmap', rmap, wmap, sliceForM_, sliceMapM_,
  readGlobal, writeGlobal, modifyGlobal,
  sliceSize,
  System(..), runSystem, runWith,
  Component(..), Entity, Slice, Has(..), Safe(..),

  -- Reader
  asks, ask, liftIO, lift,

  -- Self
  newEntity, EntityCounter, nextEntity, initCounter,
  runGC,
) where

import Prelude hiding (read, all)
import Control.Monad.Reader

import Apecs.Core as A
import Apecs.Util
import System.Mem (performMajorGC)

runGC :: System w ()
runGC = liftIO performMajorGC
