{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Provides 'ArrayMap', a direct-indexed array-backed store parameterised over the underlying
mutable vector type via 'Data.Vector.Generic.Mutable.MVector'. Both the data array and the
presence array grow on demand: when a component is set for an entity whose ID exceeds the
current capacity, both arrays are reallocated to @max (entityId + 1) (2 * capacity)@.

This gives O(1) get, set, exists, and destroy. Iteration via 'explMembers' is O(capacity)
because it must scan the presence array; prefer this store for components that are held by
the majority of entities, where that scan is not wasteful.

Convenience aliases:

* 'BArrayMap' — boxed storage via 'Data.Vector.Mutable.MVector', works for any component type.
* 'UArrayMap' — unboxed storage via 'Data.Vector.Unboxed.Mutable.MVector', requires an
  'Data.Vector.Unboxed.Unbox' instance, but is significantly more memory-efficient for
  scalar and enum types.
-}
module Apecs.Experimental.ArrayMap
  ( ArrayMap (..)
  , BArrayMap
  , UArrayMap
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Prim (RealWorld)

import Apecs.Core

initialCapacity :: Int
initialCapacity = 1024

growTo
  :: GMV.MVector v c
  => IORef (v RealWorld c)
  -> IORef (UM.IOVector Bool)
  -> Int
  -> IO ()
growTo dataRef presentRef ety = do
  oldData    <- readIORef dataRef
  oldPresent <- readIORef presentRef
  let oldCap = UM.length oldPresent
      -- When ety is within normal doubling range, double as usual.
      -- When ety is a large jump beyond 2*oldCap, allocate oldCap extra headroom
      -- beyond ety so that sequential writes from ety don't each trigger a resize.
      newCap  = max (2 * oldCap) (ety + 1 + oldCap)
      added   = newCap - oldCap
  newData    <- GMV.unsafeGrow oldData added
  newPresent <- UM.unsafeGrow oldPresent added
  -- Zero-initialise the new presence slots; unsafeGrow does not guarantee this.
  UM.set (UM.unsafeSlice oldCap added newPresent) False
  writeIORef dataRef    newData
  writeIORef presentRef newPresent

-- | Direct-indexed, dynamically growing store parameterised over the mutable vector type.
--
-- @v@ must be an instance of 'GMV.MVector'; use 'VM.MVector' (via 'BArrayMap') for any
-- component type, or 'UM.MVector' (via 'UArrayMap') for unboxed storage of scalar and
-- enum types. @amPresent@ is always an unboxed 'Bool' vector.
data ArrayMap v c = ArrayMap
  { amData    :: !(IORef (v RealWorld c))
  , amPresent :: !(IORef (UM.IOVector Bool))
  }

-- | 'ArrayMap' backed by boxed storage. Works for any component type.
type BArrayMap = ArrayMap VM.MVector

-- | 'ArrayMap' backed by unboxed storage. Requires 'U.Unbox'; significantly more
-- memory-efficient for scalar and enum component types.
type UArrayMap = ArrayMap UM.MVector

type instance Elem (ArrayMap v c) = c

instance (MonadIO m, GMV.MVector v c) => ExplInit m (ArrayMap v c) where
  explInit = liftIO $ do
    dat  <- GMV.unsafeNew initialCapacity
    pres <- UM.replicate initialCapacity False
    ArrayMap <$> newIORef dat <*> newIORef pres

instance (MonadIO m, GMV.MVector v c) => ExplGet m (ArrayMap v c) where
  {-# INLINE explGet #-}
  explGet (ArrayMap dataRef _) ety = liftIO $ do
    dat <- readIORef dataRef
    GMV.unsafeRead dat ety

  {-# INLINE explExists #-}
  explExists (ArrayMap _ presentRef) ety = liftIO $ do
    pres <- readIORef presentRef
    if ety < UM.length pres
      then UM.unsafeRead pres ety
      else pure False

instance (MonadIO m, GMV.MVector v c) => ExplSet m (ArrayMap v c) where
  {-# INLINE explSet #-}
  explSet (ArrayMap dataRef presentRef) ety x = liftIO $ do
    pres <- readIORef presentRef
    when (ety >= UM.length pres) $
      growTo dataRef presentRef ety
    dat   <- readIORef dataRef
    pres' <- readIORef presentRef
    GMV.unsafeWrite dat   ety x
    UM.unsafeWrite  pres' ety True

instance MonadIO m => ExplDestroy m (ArrayMap v c) where
  {-# INLINE explDestroy #-}
  explDestroy (ArrayMap _ presentRef) ety = liftIO $ do
    pres <- readIORef presentRef
    when (ety < UM.length pres) $
      UM.unsafeWrite pres ety False

instance MonadIO m => ExplMembers m (ArrayMap v c) where
  {-# INLINE explMembers #-}
  explMembers (ArrayMap _ presentRef) = liftIO $ do
    pres   <- readIORef presentRef
    frozen <- U.unsafeFreeze pres
    pure $! U.map fst . U.filter snd $ U.indexed frozen
