{-# LANGUAGE FlexibleContexts #-}

module Apecs (
  -- * Types
    System(..),
    Component(..), Entity(..), Slice, Has(..), Safe(..), cast,

  -- * Initializable
    initStoreWith,

  -- * HasMembers wrapper functions
    destroy, exists, owners, resetStore,

  -- * Store wrapper functions
    get, set, set', modify,
    cmap, cmapM, cmapM_, cimapM, cimapM_,
    rmap', rmap, wmap, wmap', cmap',


  -- * GlobalRW wrapper functions
    readGlobal, writeGlobal, modifyGlobal,

  -- * Query
    slice, All(..),

  -- * Other
    runSystem, runWith,

  -- All slice functions
  module SL,

  -- Reader
  asks, ask, liftIO, lift,
) where

import Control.Monad.Reader (asks, ask, liftIO, lift)

import Apecs.Types
import Apecs.System
import Apecs.Slice as SL

