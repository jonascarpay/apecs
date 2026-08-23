{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The kinematic character mover, mirroring the engine's character
sample: collide, solve planes, sweep, iterate.
-}
module Apecs.Box2D.Mover where

import Apecs
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Marshal.Utils (fromBool, toBool)

import Box2D.Callbacks (withPlaneResultFcn)
import Box2D.Collision qualified as B2Collision
import Box2D.Geometry qualified as B2Geometry
import Box2D.MathTypes (vec2Add, vec2LengthSquared, vec2Scale, vec2Sub)
import Box2D.Mover qualified as B2Mover
import Box2D.Shape (Filter)
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry
import Apecs.Box2D.Query
import Apecs.Box2D.Types

-- | @sample_character.cpp@'s @Mover::m_planeCapacity@: at most this many planes are kept per step.
planeCapacity :: Int
planeCapacity = 8

-- | @sample_character.cpp@'s outer collide\/solve\/cast loop count.
moverStepIterations :: Int
moverStepIterations = 5

-- | @sample_character.cpp@'s per-iteration break tolerance on the swept translation.
moverStepTolerance :: Float
moverStepTolerance = 0.01

{- | A 'B2Collision.PlaneResult' as a fresh 'B2Collision.CollisionPlane'
for "Box2D.Mover": no push limit (the sample's per-shape @maxPush@ user
data isn't exposed here) and velocity always clipped.
-}
mkCollisionPlane :: B2Collision.PlaneResult -> B2Collision.CollisionPlane
mkCollisionPlane pr =
  B2Collision.CollisionPlane
    { plane = pr.plane
    , pushLimit = 1 / 0
    , push = 0
    , clipVelocity = fromBool True
    }

{- | What 'moveCharacter' produced: where the mover ended up and its
velocity clipped against every surface it touched (kill the
into-the-wall component so speed doesn't build up against obstacles).
-}
data MoverResult = MoverResult
  { position :: !WVec
  , velocity :: !WVec
  }
  deriving (Eq, Show)

{- | Move a character capsule from its current position toward a target,
sliding along whatever it hits — the engine-blessed kinematic character
controller (collide → solve planes → sweep, iterated). The capsule is
given in local space like 'GeoCapsule' (two centers and a radius) and
does not need any 'Apecs.Box2D.Body.Body' or 'Shape' — the mover is pure query, it does
not push bodies around. Pass the current velocity to get it clipped
against the surfaces touched this step; integrate gravity/input into it
yourself before calling. Filter semantics match the other queries (see
'toQueryFilter').

Mirrors @sample_character.cpp@'s @Mover@ faithfully: up to 5
collide\/solve\/cast iterations, breaking early once a step's swept
translation is shorter than 0.01 units; each iteration gathers up to 8
collision planes fresh via 'B2World.collideMover', resolves the target
delta against them with the engine's own solver ("Box2D.Mover"'s
'B2Mover.solvePlanes'), and sweeps the resolved translation with
'B2World.castMover'. The final velocity is clipped
('B2Mover.clipVector') against the planes gathered in whichever
iteration ran last — same as the sample, which never clears its plane
buffer after the loop exits. Every plane is treated as unlimited push
with clipping on; the sample's per-shape @maxPush@\/@clipVelocity@ come
from shape user data, which this layer doesn't expose.
-}
moveCharacter
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => BVec
  -- ^ mover capsule center 1, local
  -> BVec
  -- ^ mover capsule center 2, local
  -> Float
  -- ^ mover capsule radius
  -> WVec
  -- ^ current position (world origin of the capsule frame)
  -> WVec
  -- ^ target position for this step
  -> WVec
  -- ^ current velocity
  -> Filter
  -> SystemT w m MoverResult
moveCharacter c1 c2 radius pos0 target vel0 fltr = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    let
      qf = toQueryFilter fltr
      capsule = B2Geometry.Capsule c1 c2 radius
    -- planes land in a buffer reused across iterations (reset before
    -- every 'B2World.collideMover' call), so the FunPtr below can be
    -- wrapped once for the whole call instead of once per iteration
    -- and no per-plane list is built.
    buf <- VSM.new planeCapacity
    countRef <- newIORef (0 :: Int)
    let visit _shapeId pr = do
          when (toBool pr.hit) $ do
            n <- readIORef countRef
            when (n < planeCapacity) $ do
              VSM.write buf n (mkCollisionPlane pr)
              writeIORef countRef (n + 1)
          pure True
    withPlaneResultFcn visit $ \fp ctx -> do
      let
        gatherPlanes pos = do
          writeIORef countRef 0
          _ <- B2World.collideMover sp.world pos capsule qf fp ctx
          n <- readIORef countRef
          VS.freeze (VSM.take n buf)

        step i pos lastPlanes
          | i >= moverStepIterations = pure (pos, lastPlanes)
          | otherwise = do
              planes <- gatherPlanes pos
              (translation, planes', _iters) <- B2Mover.solvePlanes (vec2Sub target pos) planes
              fraction <- B2World.castMover sp.world pos capsule translation qf
              let
                delta = vec2Scale fraction translation
                pos' = vec2Add pos delta
              if vec2LengthSquared delta < moverStepTolerance * moverStepTolerance then
                pure (pos', planes')
              else
                step (i + 1) pos' planes'

      (finalPos, finalPlanes) <- step (0 :: Int) pos0 VS.empty
      finalVel <- B2Mover.clipVector vel0 finalPlanes
      pure (MoverResult finalPos finalVel)
