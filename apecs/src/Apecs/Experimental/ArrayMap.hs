{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Provides 'ArrayMap', a direct-indexed array-backed store. Both the data array and the
presence array grow on demand: when a component is set for an entity whose ID exceeds the
current capacity, both arrays are reallocated to @max (entityId + 1) (2 * capacity)@.

This gives O(1) get, set, exists, and destroy. Iteration via 'explMembers' is O(capacity)
because it must scan the presence array; prefer this store for components that are held by
the majority of entities, where that scan is not wasteful.
-}
module Apecs.Experimental.ArrayMap
  ( ArrayMap (..)
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Apecs.Core

initialCapacity :: Int
initialCapacity = 1024

-- | Grow both arrays to @max (ety + 1) (2 * current capacity)@, copying existing data.
growTo
  :: IORef (VM.IOVector c)
  -> IORef (UM.IOVector Bool)
  -> Int  -- ^ entity index that triggered the grow
  -> IO ()
growTo dataRef presentRef ety = do
  oldData    <- readIORef dataRef
  oldPresent <- readIORef presentRef
  let oldCap = UM.length oldPresent
      newCap = max (ety + 1) (2 * oldCap)
  newData    <- VM.unsafeGrow oldData (newCap - oldCap)
  newPresent <- UM.unsafeGrow oldPresent (newCap - oldCap)
  -- Zero-initialise the new presence slots (unsafeGrow does not guarantee this).
  UM.set (UM.unsafeSlice oldCap (newCap - oldCap) newPresent) False
  writeIORef dataRef    newData
  writeIORef presentRef newPresent

-- | Direct-indexed, dynamically growing store backed by a pair of mutable vectors.
--
-- @amData@ stores component values at the entity's index.
-- @amPresent@ stores whether each entity slot is occupied.
data ArrayMap c = ArrayMap
  { amData    :: !(IORef (VM.IOVector c))
  , amPresent :: !(IORef (UM.IOVector Bool))
  }

type instance Elem (ArrayMap c) = c

instance (MonadIO m) => ExplInit m (ArrayMap c) where
  explInit = liftIO $ do
    dat  <- VM.new initialCapacity
    pres <- UM.replicate initialCapacity False
    ArrayMap <$> newIORef dat <*> newIORef pres

instance (MonadIO m) => ExplGet m (ArrayMap c) where
  {-# INLINE explGet #-}
  explGet (ArrayMap dataRef _) ety = liftIO $ do
    dat <- readIORef dataRef
    VM.unsafeRead dat ety

  {-# INLINE explExists #-}
  explExists (ArrayMap _ presentRef) ety = liftIO $ do
    pres <- readIORef presentRef
    if ety < UM.length pres
      then UM.unsafeRead pres ety
      else pure False

instance (MonadIO m) => ExplSet m (ArrayMap c) where
  {-# INLINE explSet #-}
  explSet (ArrayMap dataRef presentRef) ety x = liftIO $ do
    pres <- readIORef presentRef
    when (ety >= UM.length pres) $
      growTo dataRef presentRef ety
    dat <- readIORef dataRef
    VM.unsafeWrite dat     ety x
    pres' <- readIORef presentRef
    UM.unsafeWrite pres'   ety True

instance (MonadIO m) => ExplDestroy m (ArrayMap c) where
  {-# INLINE explDestroy #-}
  explDestroy (ArrayMap _ presentRef) ety = liftIO $ do
    pres <- readIORef presentRef
    when (ety < UM.length pres) $
      UM.unsafeWrite pres ety False

instance (MonadIO m) => ExplMembers m (ArrayMap c) where
  {-# INLINE explMembers #-}
  explMembers (ArrayMap _ presentRef) = liftIO $ do
    pres <- readIORef presentRef
    frozen <- U.unsafeFreeze pres
    pure $! U.map fst . U.filter snd $ U.indexed frozen
