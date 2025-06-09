{-|
This module forms the apecs Prelude.
It selectively re-exports the user-facing functions from the submodules.
-}
module Apecs (
  -- * Core types
    SystemT(..), System, Component(..), Entity(..), Has(..), Not(..),
    Get, Set, Destroy, Members,

  -- * Stores
    Map, MapWith, UMap, UMapWith, SMap, SMapWith, Unique, Global, Cache,
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
-- When using 'cmap' or 'cfold' over a tuple of components, keep in mind the
-- ordering of the tuple can have performance implications!
--
-- For tuples, the way the 'cmap' and 'cfold' work under the hood is by
-- iterating over the component in the first position, and then for each entity
-- that has that component, checking whether the entity also has the components
-- in the remaining positions. Therefore, the first component will typically be
-- the most determining factor for performance, and a good rule of thumb is to,
-- __when iterating over a tuple, put the rarest component in first position__.
--
-- Let's take a look at an example.
-- Consider a simple 2D rendering system built on top of `cmapM_`:
--
-- @
-- 'cmapM_' '$' \\(Sprite sprite, Visible) -> do
--   renderSprite sprite
-- @
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
-- While putting the rarest component first is an excellent rule of thumb, to
-- get the best possible performance, always consider how maps and folds are
-- executed under the hood, and how you can order your components to optimize
-- that process.
-- 
-- === Storage Strategies
--
-- Apecs offers several storage options for components, allowing you to tune for
-- performance.
--
-- * 'Map' (Default): The default store is now a high-performance sparse set
--   providing O(1) (amortized constant time) for nearly all operations. It is
--   suitable for the vast majority of component types.
--
-- * 'UMap' and 'SMap': For performance-critical components, you can achieve
--   even greater speed by using these specialized stores. If your component can
--   be given an instance of 'Data.Vector.Unboxed.Unbox', use 'UMap'. If it has
--   a 'Foreign.Storable.Storable' instance, use 'SMap'. These stores lay out
--   data in contiguous memory blocks, which significantly improves cache
--   performance.
--
-- * 'Cache': Previously, the `Cache` was the primary tool for achieving high
--   performance. With the new sparse-set backend for `Map`, `UMap`, and `SMap`,
--   the `Cache` is no longer necessary for these stores and is considered 
--   obsolete for this purpose. It remains available for use with custom stores
--   that may have different performance characteristics.

