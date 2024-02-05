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
    cmap, cmapIf, cmapM, cmapM_,
    cfold, cfoldM, cfoldM_, collect,
  -- ** Performance
  -- $performance

  -- * Other
    runSystem, runWith,
    runGC, EntityCounter, newEntity, newEntity_, global,
    makeWorld, makeWorldAndComponents,

  -- * Re-exports
    asks, ask, liftIO, lift, Proxy (..)
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask, asks, lift)
import           Data.Proxy

import           Apecs.Components
import           Apecs.Core
import           Apecs.Stores
import           Apecs.System
import           Apecs.TH
import           Apecs.Util

-- $performance
--
-- As seen above, @apecs@ includes a small set of combinators that enable
-- mapping/folding over components. Consider a simple 2D rendering system built
-- on top of `cmapM_`:
--
-- @
-- 'cmapM_' '$' \\(Sprite sprite, Visible) -> do
--   renderSprite sprite
-- @
--
-- This will call 'renderSprite' for all entities that have both a @Sprite@
-- component and a @Visible@ component. The @Visible@ component here is a tag
-- assigned by some other system that indicates which entities are visible to
-- the game's camera.
--
-- While this rendering system works, it could be made more efficient by
-- leveraging knowledge of how the library handles reading of tupled components.
-- The usage of 'cmapM_' here (or any of the other map/fold functions) will
-- iterate over all entities with a @Sprite@ component and filter out any of
-- these entities that do not have a @Visible@ component. Depending on the game,
-- it is reasonable to assume that there are more sprites active in the game's
-- world than sprites that are visible to the game's camera.
--
-- Swapping the component ordering in the tuple is likely to be more efficient:
--
-- @
-- 'cmapM_' '$' \\(Visible, Sprite sprite) -> do
--   renderSprite sprite
-- @
--
-- Now the system iterates over just those entities that are visible to the
-- game's camera and filters out any that do not have a @Sprite@ component.
--
-- __ When using the map or fold functions over composite components, aim    __
-- __ for placing the component that rules out the most entities the fastest __
-- __ into the first slot of the tuple. The ordering of the remaining        __
-- __ components in the tuple is irrelevant.                                 __
--
-- Note that it is not guaranteed that the component with the smallest positive
-- cardinality will rule out the most entities the fastest, as the speed of
-- iteration/filtering is driven by the data structures used by each component's
-- underlying store. You can use the notion of "smallest cardinality first" as a
-- starting point for component ordering, but if you need to eke out more
-- performance from the library, you will want to benchmark and consider the
-- underlying store types in the context of your game.
