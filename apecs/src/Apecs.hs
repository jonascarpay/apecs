{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  module Data.Proxy,
  -- * Core types
    SystemT(..), System, Component(..), Entity(..), Has(..), Not(..),
    Get, Set, Destroy, Members,

  -- * Stores
    Map, Unique, Global, Cache,
    explInit,

  -- * Systems
    get, set, getAll,
    cmap, cmapM, cmapM_,
    cfold, cfoldM, cfoldM_,
    modify, destroy, exists,

  -- * Other
    runSystem, runWith,
    runGC, EntityCounter, newEntity, global,
    makeWorld, makeWorldAndComponents,

  -- * Re-exports
    asks, ask, liftIO, lift,
) where

import           Control.Monad.Reader (ask, asks, lift, liftIO)
import           Data.Proxy

import           Apecs.Stores
import           Apecs.Reactive
import           Apecs.System
import           Apecs.TH
import           Apecs.Core
import           Apecs.Util

