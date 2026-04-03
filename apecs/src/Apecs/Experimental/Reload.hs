{-# LANGUAGE LambdaCase #-}

{-|
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Utilities for persisting an apecs world across GHCi reloads using @foreign-store@.
This is intended for development workflows only — do not use in production.

__Important__: if you change your world's type (e.g. add or remove components),
you must call 'resetWorld' or restart GHCi. The foreign store is not type-safe
across type changes.

Typical usage:

@
mainDev :: IO ()
mainDev = runReloadable initWorld someSystem
@
-}

module Apecs.Experimental.Reload
  ( runReloadable
  , runReloadableAt
  , defaultStoreIndex
  , getOrInitWorld
  , storeWorld
  , resetWorld
  , Store
  ) where

import Data.Word (Word32)
import Foreign.Store (Store (..), lookupStore, readStore, writeStore)

import Apecs.Core (SystemT)
import Apecs.System (runSystem)

-- | Retrieve or initialize a world, then run a system in it.
--
-- @
-- main = runReloadable initWorld $ do
--   newEntity (Position 0, Velocity 1)
--   cmap $ \\(Position p, Velocity v) -> Position (p + v)
-- @
{-# INLINE runReloadable #-}
runReloadable :: IO w -> SystemT w IO a -> IO a
runReloadable = runReloadableAt defaultStoreIndex

-- | Retrieve or initialize a world at a specified index, then run a system in it.
--
-- @
-- main = runReloadableAt 0 initWorld $ do
--   newEntity (Position 0, Velocity 1)
--   cmap $ \\(Position p, Velocity v) -> Position (p + v)
-- @
runReloadableAt :: Word32 -> IO w -> SystemT w IO a -> IO a
runReloadableAt idx mkWorld sys =
  getOrInitWorld idx mkWorld >>= runSystem sys

-- | Default foreign store index (0). Sufficient when only one world
-- is used in a GHCi session. Use a different index if you need
-- multiple independent worlds.
defaultStoreIndex :: Word32
defaultStoreIndex = 0

-- | Look up a world in the foreign store at the given index.
-- If one exists, return it. Otherwise, run the initialization
-- action, persist the result, and return it.
getOrInitWorld :: Word32 -> IO w -> IO w
getOrInitWorld idx mkWorld =
  lookupStore idx >>= \case
    Nothing -> resetWorld idx mkWorld
    Just store -> readStore store

-- | Discard any persisted world at the given index, initialize
-- a fresh one, persist it, and return it.
resetWorld :: Word32 -> IO w -> IO w
resetWorld idx mkWorld = do
  w <- mkWorld
  writeStore (Store idx) w
  return w

-- | Persist a world value at the given foreign store index,
-- overwriting any previous value.
storeWorld :: Word32 -> w -> IO ()
storeWorld = writeStore . Store
