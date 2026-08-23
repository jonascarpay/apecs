{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The kinematic character mover, mirroring the engine's character
sample: collide, solve planes, sweep, iterate.
-}
module Apecs.Box3D.Mover where

import Apecs
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (nullFunPtr, nullPtr)

import Box3D.Callbacks (withPlaneResultFcn)
import Box3D.Collision qualified as B3Collision
import Box3D.Geometry qualified as B3Geometry
import Box3D.MathTypes (vec3Add, vec3LengthSquared, vec3Scale, vec3Sub)
import Box3D.Mover qualified as B3Mover
import Box3D.Shape (Filter)
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry
import Apecs.Box3D.Query
import Apecs.Box3D.Types

-- | @sample.cpp@'s @CharacterMover::m_planeCapacity@: at most this many planes are kept per step. Same value as the 2D sample.
planeCapacity :: Int
planeCapacity = 8

-- | @sample.cpp@'s @Player::Update@ outer collide\/solve\/cast loop count — identical to the 2D sample.
moverStepIterations :: Int
moverStepIterations = 5

-- | @sample.cpp@'s per-iteration break tolerance on the swept translation — identical to the 2D sample.
moverStepTolerance :: Float
moverStepTolerance = 0.01

{- | A 'B3Collision.PlaneResult' as a fresh 'B3Collision.CollisionPlane' for
"Box3D.Mover": no push limit (the sample's per-shape @maxPush@ user
data isn't exposed here) and velocity always clipped.
-}
mkCollisionPlane :: B3Collision.PlaneResult -> B3Collision.CollisionPlane
mkCollisionPlane pr =
  B3Collision.CollisionPlane
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
does not need any 'Apecs.Box3D.Body.Body' or 'Shape' — the mover is pure query, it does
not push bodies around. Pass the current velocity to get it clipped
against the surfaces touched this step; integrate gravity/input into it
yourself before calling. Filter semantics match the other queries (see
'toQueryFilter').

Mirrors @samples/sample.cpp@'s @Player@ character controller faithfully
— its collide\/solve\/cast loop is structurally identical to the 2D
@sample_character.cpp@ @Mover@, with the same constants (5 outer
iterations breaking below a 0.01-unit translation, up to 8 planes per
step): each iteration gathers fresh collision planes via
'B3World.collideMover', resolves the target delta against them with
the engine's own solver ("Box3D.Mover"'s 'B3Mover.solvePlanes'), and
sweeps the resolved translation with 'B3World.castMover'. The final
velocity is clipped ('B3Mover.clipVector') against the planes gathered
in whichever iteration ran last — same as the sample, which never
clears its plane buffer after the loop exits. Every plane is treated as
unlimited push with clipping on; the sample's per-shape
@maxPush@\/@clipVelocity@ come from shape user data, which this layer
doesn't expose.

'B3World.castMover' takes an optional per-shape mover filter callback;
this layer passes none (a null function pointer, which the engine
tolerates — see @b3World_CastMover@'s @fcn@ parameter), so per-shape
mover-cast filtering beyond 'Filter' bits isn't available through this
function.
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
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let capsule = B3Geometry.Capsule c1 c2 radius
    -- planes land in a buffer reused across iterations (reset before
    -- every 'B3World.collideMover' call), so the FunPtr below can be
    -- wrapped once for the whole call instead of once per iteration
    -- and no per-plane list is built.
    buf <- VSM.new planeCapacity
    countRef <- newIORef (0 :: Int)
    let visit _shapeId prs = do
          n0 <- readIORef countRef
          let room = VS.take (planeCapacity - n0) prs
          VS.imapM_ (\i pr -> VSM.write buf (n0 + i) (mkCollisionPlane pr)) room
          writeIORef countRef (n0 + VS.length room)
          pure True
    withPlaneResultFcn visit $ \fp ctx -> do
      let
        gatherPlanes pos = do
          writeIORef countRef 0
          _ <- B3World.collideMover sp.world pos capsule qf fp ctx
          n <- readIORef countRef
          VS.freeze (VSM.take n buf)

        step i pos lastPlanes
          | i >= moverStepIterations = pure (pos, lastPlanes)
          | otherwise = do
              planes <- gatherPlanes pos
              (translation, planes', _iters) <- B3Mover.solvePlanes (vec3Sub target pos) planes
              fraction <- B3World.castMover sp.world pos capsule translation qf nullFunPtr nullPtr
              let
                delta = vec3Scale fraction translation
                pos' = vec3Add pos delta
              if vec3LengthSquared delta < moverStepTolerance * moverStepTolerance then
                pure (pos', planes')
              else
                step (i + 1) pos' planes'

      (finalPos, finalPlanes) <- step (0 :: Int) pos0 VS.empty
      finalVel <- B3Mover.clipVector vel0 finalPlanes
      pure (MoverResult finalPos finalVel)
