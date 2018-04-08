{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Core types
    System(..), Component(..), Entity(..), Has(..), Not(..),

  -- * Stores
    Map, Unique, Global, Cache,
    initStore,

  -- * Systems
    get, set, getAll,
    cmap, cmapM, cmapM_,
    modify, destroy, exists,

  -- * Other
    runSystem, runWith,
    runGC, EntityCounter, newEntity, global, proxy,
    makeWorld, makeWorldAndComponents,

  -- * Re-exports
    asks, ask, liftIO, lift,
) where

import           Control.Monad.Reader (ask, asks, lift, liftIO)

import           Apecs.Stores
import           Apecs.System
import           Apecs.TH
import           Apecs.Core
import           Apecs.Util

