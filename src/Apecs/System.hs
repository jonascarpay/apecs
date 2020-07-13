{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Apecs.System
  ( cmap,
    cmapM_,
  )
where

import Apecs.Core
import Control.Monad
import Data.Proxy
import qualified Data.Vector.Storable as V

{-# INLINE cmap #-}
cmap ::
  forall w m cx cy.
  (Get w m cx, Set w m cy, Members w m cx, Monad m) =>
  (cx -> cy) ->
  SystemT w m ()
cmap f = do
  etys <- members (Proxy @cx)
  V.forM_ etys $ \ety -> do
    x <- get ety
    set (f x) ety

{-# INLINE cmapM_ #-}
cmapM_ ::
  forall w m c.
  (Get w m c, Members w m c, Monad m) =>
  (c -> SystemT w m ()) ->
  SystemT w m ()
cmapM_ f = do
  etys <- members (Proxy @c)
  V.forM_ etys $ get >=> f
