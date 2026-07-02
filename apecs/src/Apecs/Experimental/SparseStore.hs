{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Stability: experimental

This module is experimental, and its API might change between point releases. Use at your own risk.

Provides 'SparseStore', a store with O(1) per-entity access and O(count) iteration.

Two arrays are maintained in parallel:

* A /sparse/ index array, indexed by entity ID, holds the dense-array index
  for each live component (-1 means absent). It grows on demand exactly like
  'ArrayMap'.
* A /dense/ data array (and a parallel entity-ID array) is kept fully packed
  with no gaps. Destroy uses swap-and-pop to maintain this invariant.

Because 'explMembers' just returns the packed entity-ID array, iteration is
O(count) with excellent cache locality — components are visited in a tight loop
over contiguous memory rather than scattered across a sparse presence scan.

Prefer this store over 'Cache' or 'ArrayMap' when:

* The component is sparse (only a fraction of entities hold it) and
  you iterate over all holders frequently ('cmap', 'cfold', etc.).
* You can tolerate the extra memory for the sparse index array
  (∝ max entity ID, not count).

O(1) get, set, exists, destroy. O(count) members.

Convenience aliases:

* 'SparseStoreB' — boxed dense array, works for any component type.
* 'SparseStoreU' — unboxed dense array, requires 'Data.Vector.Unboxed.Unbox'.
-}
module Apecs.Experimental.SparseStore
  ( SparseStore (..)
  , SparseStoreB
  , SparseStoreU
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

initialSparse, initialDense :: Int
initialSparse = 1024
initialDense = 256

absent :: Int
absent = -1

-- | Sparse-set store parameterised over the mutable vector type for the dense array.
data SparseStore v c = SparseStore
  { ssIndices :: !(IORef (UM.IOVector Int)) -- sparse: entity → dense index, absent = -1
  , ssDense :: !(IORef (v RealWorld c)) -- packed component values
  , ssEntities :: !(IORef (UM.IOVector Int)) -- packed entity IDs, parallel to ssDense
  , ssCount :: !(IORef Int)
  }

-- | 'SparseStore' backed by boxed storage. Works for any component type.
type SparseStoreB = SparseStore VM.MVector

-- | 'SparseStore' backed by unboxed storage. Requires 'U.Unbox'.
type SparseStoreU = SparseStore UM.MVector

type instance Elem (SparseStore v c) = c

-- Grow the sparse index array to accommodate entity @ety@, initialising new slots to -1.
growSparse :: IORef (UM.IOVector Int) -> Int -> IO (UM.IOVector Int)
growSparse indRef ety = do
  inds <- readIORef indRef
  let
    oldCap = UM.length inds
    newCap = max (2 * oldCap) (ety + 1 + oldCap)
    added = newCap - oldCap
  inds' <- UM.unsafeGrow inds added
  UM.set (UM.slice oldCap added inds') absent
  writeIORef indRef inds'
  pure inds'

-- Double the capacity of both dense arrays together.
growDense :: (GMV.MVector v c) => IORef (v RealWorld c) -> IORef (UM.IOVector Int) -> IO ()
growDense denseRef entRef = do
  d <- readIORef denseRef
  e <- readIORef entRef
  let extra = GMV.length d
  d' <- GMV.unsafeGrow d extra
  e' <- UM.unsafeGrow e extra
  writeIORef denseRef d'
  writeIORef entRef e'

instance (MonadIO m, GMV.MVector v c) => ExplInit m (SparseStore v c) where
  explInit = liftIO $ do
    inds <- UM.replicate initialSparse absent
    dense <- GMV.unsafeNew initialDense
    ents <- UM.new initialDense
    SparseStore
      <$> newIORef inds
      <*> newIORef dense
      <*> newIORef ents
      <*> newIORef 0

instance (MonadIO m, GMV.MVector v c) => ExplGet m (SparseStore v c) where
  {-# INLINE explGet #-}
  explGet (SparseStore indRef denseRef _ _) ety = liftIO $ do
    inds <- readIORef indRef
    di <- UM.unsafeRead inds ety
    dense <- readIORef denseRef
    GMV.unsafeRead dense di

  {-# INLINE explExists #-}
  explExists (SparseStore indRef _ _ _) ety = liftIO $ do
    inds <- readIORef indRef
    if ety >= UM.length inds then
      pure False
    else
      (/= absent) <$> UM.unsafeRead inds ety

instance (MonadIO m, GMV.MVector v c) => ExplSet m (SparseStore v c) where
  {-# INLINE explSet #-}
  explSet (SparseStore indRef denseRef entRef cntRef) ety x = liftIO $ do
    inds <- readIORef indRef
    inds' <-
      if ety >= UM.length inds then
        growSparse indRef ety
      else
        pure inds
    di <- UM.unsafeRead inds' ety
    if di /= absent then do
      -- component already present: update in place, no index changes needed
      dense <- readIORef denseRef
      GMV.unsafeWrite dense di x
    else do
      -- new component: append to the dense arrays
      cnt <- readIORef cntRef
      dense <- readIORef denseRef
      when (cnt >= GMV.length dense) $
        growDense denseRef entRef
      dense' <- readIORef denseRef
      ents' <- readIORef entRef
      GMV.unsafeWrite dense' cnt x
      UM.unsafeWrite ents' cnt ety
      UM.unsafeWrite inds' ety cnt
      writeIORef cntRef (cnt + 1)

instance (MonadIO m, GMV.MVector v c) => ExplDestroy m (SparseStore v c) where
  {-# INLINE explDestroy #-}
  explDestroy (SparseStore indRef denseRef entRef cntRef) ety = liftIO $ do
    inds <- readIORef indRef
    when (ety < UM.length inds) $ do
      di <- UM.unsafeRead inds ety
      when (di /= absent) $ do
        cnt <- readIORef cntRef
        let lastIdx = cnt - 1
        dense <- readIORef denseRef
        ents <- readIORef entRef
        -- swap the slot being removed with the last slot, then shrink
        when (di /= lastIdx) $ do
          lastVal <- GMV.unsafeRead dense lastIdx
          lastEty <- UM.unsafeRead ents lastIdx
          GMV.unsafeWrite dense di lastVal
          UM.unsafeWrite ents di lastEty
          UM.unsafeWrite inds lastEty di
        UM.unsafeWrite inds ety absent
        writeIORef cntRef lastIdx

instance (MonadIO m) => ExplMembers m (SparseStore v c) where
  {-# INLINE explMembers #-}
  explMembers (SparseStore _ _ entRef cntRef) = liftIO $ do
    cnt <- readIORef cntRef
    ents <- readIORef entRef
    U.freeze (UM.slice 0 cnt ents)
