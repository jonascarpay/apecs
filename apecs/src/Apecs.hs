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
    runSystemM, runWithM,
    runGC, EntityCounter, newEntity, newEntity_, global,
    makeWorld, makeWorldAndComponents,

  -- * Re-exports
    asks, ask, liftIO, S.lift, Proxy (..)
) where

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as S
import           Data.Proxy

import           Apecs.Components
import           Apecs.Core
import           Apecs.Stores
import           Apecs.System
import           Apecs.TH
import           Apecs.Util

ask :: S.MonadState a f => f a
ask = S.get

asks :: S.MonadState a f => (a -> b) -> f b
asks f = f <$> S.get
