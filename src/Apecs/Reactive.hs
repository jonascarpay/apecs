{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Apecs.Reactive
  ( SpatialHash,
    inverseGet,
    inverseGetLocal,
  )
where

import Apecs.Core
import Apecs.Focus
import Control.Monad.Reader
import qualified Control.Monad.State as S
import qualified Data.Array.IO as A
import qualified Data.IntSet as IS
import Data.Ix

data SpatialHash c s = SpatialHash
  { _shTable :: A.IOArray c IS.IntSet,
    _shChild :: s
  }

shChild :: Lens' (SpatialHash c s) s
shChild f (SpatialHash t c) = SpatialHash t <$> f c

type instance Components (SpatialHash c s) = Components s

{-# INLINE inverseGetLocal #-}
inverseGetLocal :: (Ix c, MonadIO m) => c -> SystemT (SpatialHash c s) m [Entity]
inverseGetLocal c = do
  t <- S.gets _shTable
  liftIO $ fmap Entity . IS.toList <$> A.readArray t c

inverseGet ::
  forall w m s c.
  ( Ix c,
    MonadIO m,
    HasStore w c,
    GetStore w c ~ SpatialHash c s
  ) =>
  c ->
  SystemT w m [Entity]
inverseGet c = focusStore @c $ inverseGetLocal c

{-# INLINE register #-}
register :: (Ix c, MonadIO m) => c -> Entity -> SystemT (SpatialHash c s) m ()
register c (Entity ety) = do
  table <- S.gets _shTable
  liftIO $ A.readArray table c >>= A.writeArray table c . IS.insert ety

{-# INLINE deregister #-}
deregister :: (Ix c, MonadIO m) => c -> Entity -> SystemT (SpatialHash c s) m ()
deregister c (Entity ety) = do
  table <- S.gets _shTable
  liftIO $ A.readArray table c >>= A.writeArray table c . IS.delete ety

instance (Ix c, Bounded c, MonadIO m, Initialize m s) => Initialize m (SpatialHash c s) where
  initialize = do
    t <- liftIO $ A.newArray (minBound, maxBound) mempty
    SpatialHash t <$> initialize

instance (Functor m, Get s m c) => Get (SpatialHash c' s) m c where
  get = zoom shChild . get
  exists p = zoom shChild . exists p

instance (Functor m, Members s m c) => Members (SpatialHash c' s) m c where
  members = zoom shChild . members

instance (Get s m c, Set s m c, Ix c, MonadIO m) => Set (SpatialHash c s) m c where
  -- TODO:
  -- Smarter (de)registration?
  -- Do we check if the underlying store actually works properly?
  --   Use Cacheable to ensure well-behavedness?
  --   SpatialHash is not cacheable itself, so precludes nesting?
  --   SpatialHash is not cacheable itself, so precludes nesting?
  -- This also goes for Destroy
  set c ety = do
    old :: Maybe c <- zoom shChild $ get ety
    forM_ old $ \c' -> deregister c' ety
    register c ety
    zoom shChild $ set c ety

instance (Functor m, Set s m c) => Set (SpatialHash d s) m c where
  set c = zoom shChild . set c

instance (Ix c, Get s m c, MonadIO m, Destroy s m c) => Destroy (SpatialHash c s) m c where
  destroy p ety = do
    old :: Maybe c <- zoom shChild $ get ety
    forM_ old $ \c' -> deregister c' ety
    zoom shChild $ destroy p ety

instance (Functor m, Destroy s m c) => Destroy (SpatialHash d s) m c where
  destroy p = zoom shChild . destroy p
