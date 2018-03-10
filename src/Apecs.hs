{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Types
    System,
    Component(..), Entity(..), Has(..),
    Not(..),

    Map, Unique, Global,

  -- * Store wrapper functions
    initStore,
    get, get', set,
    cmap, cmapM, cmapM_,
    modify, destroy, exists,

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

