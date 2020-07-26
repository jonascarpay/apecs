{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Apecs.System
  ( cmap,
    cmapIf,
    cmapM,
    cmapM_,
  )
where

import Apecs.Core
import Control.Monad
import Data.Proxy
import qualified Data.Vector.Storable as V

{-# INLINE forMembers_ #-}
forMembers_ ::
  (Monad m, Members w m c) =>
  Proxy c ->
  (Entity -> SystemT w m a) ->
  SystemT w m ()
forMembers_ p f = members p >>= V.mapM_ f

{-# INLINE cmap #-}
cmap ::
  forall w m cx cy.
  (Get w m cx, Set w m cy, Members w m cx, Monad m) =>
  (cx -> cy) ->
  SystemT w m ()
cmap f =
  forMembers_ (Proxy @cx) $ \ety -> do
    x <- get ety
    set (f x) ety

{-# INLINE cmapM_ #-}
cmapM_ ::
  forall w m c.
  (Get w m c, Members w m c, Monad m) =>
  (c -> SystemT w m ()) ->
  SystemT w m ()
cmapM_ f = forMembers_ (Proxy @c) $ get >=> f

{-# INLINE cmapM #-}
cmapM ::
  forall w m cx cy.
  (Get w m cx, Set w m cy, Members w m cx, Monad m) =>
  (cx -> SystemT w m cy) ->
  SystemT w m ()
cmapM f =
  forMembers_ (Proxy @cx) $ \ety -> do
    x <- get ety
    y <- f x
    set y ety

{-# INLINE cmapIf #-}
cmapIf ::
  forall w m cp cx cy.
  (Get w m cx, Get w m cp, Set w m cy, Members w m cp, Monad m) =>
  (cp -> Bool) ->
  (cx -> cy) ->
  SystemT w m ()
cmapIf cond f = forMembers_ (Proxy @(cp, cx)) $ \ety -> do
  p <- get ety
  when (cond p) $ do
    x <- get ety
    set (f x) ety

-- cfold ::
--   forall w m c a.
--   (Members w m c, Get w m c) =>
--   (a -> c -> a) ->
--   a ->
--   SystemT w m a
-- cfold f a0 = do
--   members (Proxy @c) >>= mapM_ $ \ety -> undefined
