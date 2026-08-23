{-# LANGUAGE OverloadedRecordDot #-}

{-| Engine debug drawing over the store's world: shapes, joints, contacts,
bounds and the other overlays 'DebugDraw' can switch on. Complements the
component-driven rendering in apecs-gloss with what only the engine
knows.
-}
module Apecs.Box2D.Debug
  ( debugDraw
  , debugDrawCommands
  , Draw (..)
  , noDraw
  , Cmd (..)
  , DebugDraw (..)
  , defaultDebugDraw
  , toWorld
  ) where

import Apecs
import Control.Monad.IO.Class (MonadIO)

import Box2D.Callbacks (Cmd (..), Draw (..), noDraw, toWorld)
import Box2D.Callbacks qualified as B2Callbacks
import Box2D.DebugDraw (DebugDraw (..), defaultDebugDraw)

import Apecs.Box2D.Types

{- | Walk the world through the engine's debug renderer, calling the 'Draw'
slots for every primitive the 'DebugDraw' flags select; start from
'defaultDebugDraw' and set the @draw*@ flags you want. The engine is
midway through iterating the world while the slots run, so do not
touch components or the store from inside them — use
'debugDrawCommands' if the renderer needs to. An exception thrown by a
slot surfaces here.
-}
debugDraw :: forall w m. (MonadIO m, Has w m Physics) => Draw -> DebugDraw -> SystemT w m ()
debugDraw d base = do
  sp :: B2Space Physics <- getStore
  liftIO $ B2Callbacks.withDraw d base (B2Callbacks.draw sp.world)

{- | Collect one frame of debug primitives, in engine order, to render once
the world is free again. Costs one list per frame over 'debugDraw', but
the rendering code can do anything it likes with the store.
-}
debugDrawCommands :: forall w m. (MonadIO m, Has w m Physics) => DebugDraw -> SystemT w m [Cmd]
debugDrawCommands base = do
  sp :: B2Space Physics <- getStore
  liftIO $ snd <$> B2Callbacks.withRecorder base (B2Callbacks.draw sp.world)
