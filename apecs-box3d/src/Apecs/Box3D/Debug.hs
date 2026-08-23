{-# LANGUAGE OverloadedRecordDot #-}

{-| Engine debug drawing over the store's world: joints, contacts, bounds,
transforms and the other overlays 'DebugDraw' can switch on. Complements
the component-driven rendering in apecs-gloss-3d with what only the
engine knows.

Shapes themselves are drawn through the world's debug-shape callbacks
(@WorldDef.createDebugShape@), which the store does not install, so the
'shape' slot only fires for worlds bootstrapped through
'Apecs.Box3D.Space.initPhysicsWith' with a 'WorldDef' that carries them —
see 'Box3D.Callbacks.withDebugShapes'.
-}
module Apecs.Box3D.Debug
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
import Data.Word (Word64)

import Box3D.Callbacks (Cmd (..), Draw (..), noDraw, toWorld)
import Box3D.Callbacks qualified as B3Callbacks
import Box3D.DebugDraw (DebugDraw (..), defaultDebugDraw)

import Apecs.Box3D.Types

{- | Walk the world through the engine's debug renderer, calling the 'Draw'
slots for every primitive the 'DebugDraw' flags select; start from
'defaultDebugDraw' and set the @draw*@ flags you want. Only shapes whose
category matches the mask are visited; pass 'maxBound' for all of them.
The engine is midway through iterating the world while the slots run,
so do not touch components or the store from inside them — use
'debugDrawCommands' if the renderer needs to. An exception thrown by a
slot surfaces here.
-}
debugDraw :: forall w m. (MonadIO m, Has w m Physics) => Draw -> DebugDraw -> Word64 -> SystemT w m ()
debugDraw d base mask = do
  sp :: B3Space Physics <- getStore
  liftIO $ B3Callbacks.withDraw d base (B3Callbacks.draw sp.world mask)

{- | Collect one frame of debug primitives, in engine order, to render once
the world is free again. Costs one list per frame over 'debugDraw', but
the rendering code can do anything it likes with the store.
-}
debugDrawCommands :: forall w m. (MonadIO m, Has w m Physics) => DebugDraw -> Word64 -> SystemT w m [Cmd]
debugDrawCommands base mask = do
  sp :: B3Space Physics <- getStore
  liftIO $ snd <$> B3Callbacks.withRecorder base (B3Callbacks.draw sp.world mask)
