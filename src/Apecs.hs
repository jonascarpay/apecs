{-# LANGUAGE FlexibleContexts #-}

module Apecs (
  -- Core
  initStore,
  destroy, exists, sliceOwners,
  get, set, setMaybe, modify,
  cmap, rmap', rmap, wmap, wmap', sliceForM_, sliceForMC_, sliceMapM_, sliceMapMC_,
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
