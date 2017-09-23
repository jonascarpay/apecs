{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Types
    System(..),
    Component(..), Entity(..), Slice, Has(..), Safe(..), cast,
    Map, Set, Unique, Global, Flag(..),


  -- * Initializable
    initStoreWith,

  -- ** HasMembers wrapper functions
    destroy, exists, owners, resetStore,

  -- ** Store wrapper functions
    get, set, set', modify,
    cmap, cmapM, cmapM_, cimapM, cimapM_,
    rmap', rmap, wmap, wmap', cmap',


  -- ** GlobalRW wrapper functions
    readGlobal, writeGlobal, modifyGlobal,

  -- * Other
    runSystem, runWith,
    initStore, runGC, EntityCounter, initCounter, newEntity,

  -- All slice functions
  module SL,

  -- Reader
  asks, ask, liftIO, lift,
) where

import Control.Monad.Reader (asks, ask, liftIO, lift)

import Apecs.Types
import Apecs.System
import Apecs.Slice as SL
import Apecs.Stores
import Apecs.Util

