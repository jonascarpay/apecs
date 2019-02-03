{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Core types
    SystemT(..), System, Component(..), Entity(..), Has(..), Not(..),
    Get, Set, Destroy, Members,

  -- * Stores
    Map, Unique, Global, Cache,
    explInit,

  -- * Systems
    get, set, ($=),
    destroy, exists,
    modify, ($~), 
    cmap,  cmapM,  cmapM_,
    cfold, cfoldM, cfoldM_,

  -- * Other
    runSystem, runWith,
    runGC, EntityCounter, newEntity, global,
    makeWorld, makeWorldAndComponents,

  -- * Re-exports
    asks, ask, liftIO, lift, Proxy (..)
) where

import           Control.Monad.Reader (ask, asks, lift, liftIO)
import           Data.Proxy

import           Apecs.Stores
import           Apecs.Components
import           Apecs.System
import           Apecs.TH
import           Apecs.Core
import           Apecs.Util

