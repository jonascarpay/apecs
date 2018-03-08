{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Types
    System(..),
    Component(..), Entity(..), Slice, Has(..), Safe(..), cast,
    Map, Unique, Global,

  -- * Store wrapper functions
    initStore,
    destroy, exists, owners, resetStore,
    get, getUnsafe, set, set', modify,
    cmap, cmapM, cmapM_, cimapM, cimapM_,
    rmap', rmap, wmap, wmap', cmap',

  -- ** GlobalRW wrapper functions
    getGlobal, setGlobal, modifyGlobal,

  -- * Other
    runSystem, runWith,
    runGC, EntityCounter, newEntity,
    makeWorld,

  -- * Re-exports
    asks, ask, liftIO, lift,
) where

import           Control.Monad.Reader (ask, asks, lift, liftIO)

import           Apecs.Stores
import           Apecs.System
import           Apecs.TH
import           Apecs.Types
import           Apecs.Util

