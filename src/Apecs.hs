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
  Component(..), Entity, Slice, Has(..), Safe(..), cast,

  -- Reader
  asks, ask, liftIO, lift,

  -- Self
  newEntity, EntityCounter, nextEntity, initCounter,
  runGC,
) where

import Apecs.Core as A
import Apecs.Util
import Control.Monad.Reader (asks, ask, liftIO, lift)
