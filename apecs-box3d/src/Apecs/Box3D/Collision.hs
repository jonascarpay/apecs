{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Collision, impact, sensor, joint and body-move events from the
last 'Apecs.Box3D.Space.stepPhysics', read as global components.
-}
module Apecs.Box3D.Collision where

import Apecs
import Apecs.Core
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector.Storable qualified as VS
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (toBool)

import Box3D.Body qualified as B3Body
import Box3D.Collision qualified
import Box3D.Contact qualified as B3Contact
import Box3D.Events qualified as B3Events
import Box3D.Id (BodyId, ShapeId)
import Box3D.MathTypes (Quat, Transform (..), vec3Add, vec3Zero)
import Box3D.Shape qualified as B3Shape
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

{- | A contact manifold: the surface normal and the world contact
points. Box3D uses speculative contacts, so a begin-touch manifold can
contain slightly separated points (positive separation) and can even
momentarily have no points.
-}
data ContactManifold = ContactManifold
  { normal :: !WVec
  -- ^ Contact normal, pointing from A to B.
  , points :: ![WVec]
  -- ^ World contact points, up to 4 per 3D manifold.
  }
  deriving (Eq, Show)

{- | A contact from the last 'Apecs.Box3D.Space.stepPhysics': the shapes involved, the
bodies they hang off and — for begin-touch events — the contact
manifolds; convex-pair contacts carry one, mesh and height-field
contacts can carry several (one per touched region). 'CollisionsEnd'
events never carry manifolds; a begin-touch event lacks them when its
contact died between the step and the read (a shape destroyed after
the step) — and, unlike 2D's 'Maybe' encoding, an empty list also
covers the rare live speculative contact the engine reports with zero
manifolds, so the two cases are not distinguishable here.

Equality compares the participants only, ignoring the manifolds, so a
begin-touch value and the end-touch value of the same contact compare
equal — active-contact bookkeeping can pair them up with e.g.
'Data.List.delete'.
-}
data Collision = Collision
  { bodyA :: !Entity
  , shapeA :: !Entity
  , bodyB :: !Entity
  , shapeB :: !Entity
  , manifolds :: ![ContactManifold]
  }
  deriving (Show)

instance Eq Collision where
  a == b =
    (a.bodyA, a.shapeA, a.bodyB, a.shapeB)
      == (b.bodyA, b.shapeA, b.bodyB, b.shapeB)

{- | The (shape, body) entities behind both shapes of a pair event, if
both are still alive and registered — the resolution 'toCollision',
'toSensorEvent' and 'Impacts' share.
-}
resolvePair :: IntMap ShapeRecord -> ShapeId -> ShapeId -> IO (Maybe ((Entity, Entity), (Entity, Entity)))
resolvePair shapes sA sB = do
  ma <- shapeEntitiesIn shapes sA
  mb <- shapeEntitiesIn shapes sB
  pure ((,) <$> ma <*> mb)

{- | The shape/body entities behind a contact's two shape ids, if both
are still alive and registered; no manifolds.
-}
toCollision :: IntMap ShapeRecord -> ShapeId -> ShapeId -> IO (Maybe Collision)
toCollision shapes sA sB =
  fmap (\((sa, ba), (sb, bb)) -> Collision ba sa bb sb [])
    <$> resolvePair shapes sA sB

{- | 'toCollision' for a begin-touch event, with every contact manifold
filled in: per manifold, the normal plus the world position of each
manifold point (body A's world center of mass plus the point's A-side
anchor). If the contact is no longer valid (a shape was destroyed after
the step) the manifold list stays empty.
-}
toBeginCollision :: IntMap ShapeRecord -> IntMap BodyId -> B3World.ContactBeginTouchEvent -> IO (Maybe Collision)
toBeginCollision shapes bodies ev = do
  let contact = ev.contactId
  valid <- B3Contact.isValid contact
  if not valid then
    toCollision shapes ev.shapeIdA ev.shapeIdB
  else do
    cd <- B3Contact.getData contact
    -- resolve from the ContactData's own shape order, so the A/B
    -- entities stay consistent with the manifold normals' A-to-B
    -- orientation
    mc <- toCollision shapes cd.shapeIdA cd.shapeIdB
    if cd.manifoldCount < 1 then
      pure mc
    else forM mc $ \c -> do
      -- copy the manifolds right away: the array is engine-owned and
      -- only valid until the next step
      ms <- peekArray (fromIntegral cd.manifoldCount) cd.manifolds
      comA <-
        if all (VS.null . (.points)) ms then
          -- no points anywhere (speculative contact): the center of
          -- mass would go unused, skip both FFI calls
          pure vec3Zero
        else do
          -- body A's world center of mass turns the manifolds' A-side
          -- anchors into world points; its id comes from the body
          -- registry (toCollision just resolved the entity) rather
          -- than a getBody FFI round-trip
          let Entity bIx = c.bodyA
          bodyAId <- maybe (B3Shape.getBody cd.shapeIdA) pure (IM.lookup bIx bodies)
          B3Body.getWorldCenter bodyAId
      let toManifold m =
            ContactManifold
              m.normal
              (map (vec3Add comA . (.anchorA)) (VS.toList m.points))
      pure c{manifolds = map toManifold ms}

{- | The begin-touch contacts of the last 'Apecs.Box3D.Space.stepPhysics', a read-only
global: @Collisions touches <- get global@ after stepping. Shapes
created by this layer opt into contact events; events whose shapes
were destroyed since the step are dropped.
-}
newtype Collisions = Collisions [Collision]
  deriving (Show)

instance Component Collisions where
  type Storage Collisions = B3Space Collisions

instance (MonadIO m, Has w m Physics) => Has w m Collisions where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Collisions) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactBeginTouchEvents sp.world
    shapes <- readIORef sp.shapes
    bodies <- readIORef sp.bodies
    Collisions <$> collectEvents (toBeginCollision shapes bodies) evs

{- | The end-touch contacts of the last 'Apecs.Box3D.Space.stepPhysics', a read-only
global: @CollisionsEnd separations <- get global@ after stepping — the
counterpart of 'Collisions' for contacts that stopped touching. Events
whose shapes were destroyed since the step are dropped; this bites
harder here than for begin-touch, since destroying a shape mid-contact
drops its end event — clean up any per-contact bookkeeping when
destroying shapes. End events carry no manifolds ('manifolds'
is empty).
-}
newtype CollisionsEnd = CollisionsEnd [Collision]
  deriving (Show)

instance Component CollisionsEnd where
  type Storage CollisionsEnd = B3Space CollisionsEnd

instance (MonadIO m, Has w m Physics) => Has w m CollisionsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CollisionsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactEndTouchEvents sp.world
    shapes <- readIORef sp.shapes
    CollisionsEnd <$> collectEvents (\ev -> toCollision shapes ev.shapeIdA ev.shapeIdB) evs

{- | An above-threshold impact from the last 'Apecs.Box3D.Space.stepPhysics': the entities
involved, the world-space contact point, the contact normal (pointing
from A to B) and the approach speed. Only generated when the approach
speed exceeds the world's hit-event threshold (engine default 1;
tune with 'Apecs.Box3D.Space.HitEventThreshold').
-}
data Impact = Impact
  { bodyA :: !Entity
  , shapeA :: !Entity
  , bodyB :: !Entity
  , shapeB :: !Entity
  , point :: !WVec
  , normal :: !WVec
  , speed :: !Float
  }
  deriving (Eq, Show)

{- | The impacts of the last 'Apecs.Box3D.Space.stepPhysics', a read-only global:
@Impacts hits <- get global@ after stepping.
-}
newtype Impacts = Impacts [Impact]
  deriving (Show)

instance Component Impacts where
  type Storage Impacts = B3Space Impacts

instance (MonadIO m, Has w m Physics) => Has w m Impacts where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Impacts) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.contactHitEvents sp.world
    shapes <- readIORef sp.shapes
    Impacts
      <$> collectEvents
        ( \ev ->
            fmap
              ( \((sa, ba), (sb, bb)) ->
                  Impact
                    { bodyA = ba
                    , shapeA = sa
                    , bodyB = bb
                    , shapeB = sb
                    , point = ev.point
                    , normal = ev.normal
                    , speed = ev.approachSpeed
                    }
              )
              <$> resolvePair shapes ev.shapeIdA ev.shapeIdB
        )
        evs

{- | A sensor overlap that began or ended during the last 'Apecs.Box3D.Space.stepPhysics':
the 'Apecs.Box3D.Shape.Sensor' shape (and the body it hangs off) and the visitor shape
(and its body) that overlapped it.
-}
data SensorEvent = SensorEvent
  { sensorBody :: !Entity
  , sensorShape :: !Entity
  , visitorBody :: !Entity
  , visitorShape :: !Entity
  }
  deriving (Eq, Show)

-- | The shape/body entities behind a sensor overlap's two shape ids, if both are still alive and registered.
toSensorEvent :: IntMap ShapeRecord -> ShapeId -> ShapeId -> IO (Maybe SensorEvent)
toSensorEvent shapes sensorS visitorS =
  fmap (\((sShape, sBody), (vShape, vBody)) -> SensorEvent sBody sShape vBody vShape)
    <$> resolvePair shapes sensorS visitorS

{- | The sensor overlaps that began during the last 'Apecs.Box3D.Space.stepPhysics', a
read-only global: @SensorEvents begins <- get global@ after stepping —
the sensor counterpart of 'Collisions', with 'SensorEventsEnd' as the
ending side. Shapes created by this layer opt into sensor events, both
as sensors and as visitors; events whose sensor or visitor shape was
destroyed since the step are dropped.
-}
newtype SensorEvents = SensorEvents [SensorEvent]
  deriving (Show)

instance Component SensorEvents where
  type Storage SensorEvents = B3Space SensorEvents

instance (MonadIO m, Has w m Physics) => Has w m SensorEvents where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SensorEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.sensorBeginTouchEvents sp.world
    shapes <- readIORef sp.shapes
    SensorEvents <$> collectEvents (\ev -> toSensorEvent shapes ev.sensorShapeId ev.visitorShapeId) evs

{- | The sensor overlaps that ended during the last 'Apecs.Box3D.Space.stepPhysics', the
counterpart of 'SensorEvents' — split so a reader pays only for the
buffer it consumes. Destroying a shape mid-overlap drops its end event,
same caveat as 'CollisionsEnd'.
-}
newtype SensorEventsEnd = SensorEventsEnd [SensorEvent]
  deriving (Show)

instance Component SensorEventsEnd where
  type Storage SensorEventsEnd = B3Space SensorEventsEnd

instance (MonadIO m, Has w m Physics) => Has w m SensorEventsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SensorEventsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.sensorEndTouchEvents sp.world
    shapes <- readIORef sp.shapes
    SensorEventsEnd <$> collectEvents (\ev -> toSensorEvent shapes ev.sensorShapeId ev.visitorShapeId) evs

{- | The joints whose force or torque threshold ('Apecs.Box3D.Joint.JointForceThreshold',
'Apecs.Box3D.Joint.JointTorqueThreshold') was exceeded during the last 'Apecs.Box3D.Space.stepPhysics', a
read-only global: @JointEvents overloaded <- get global@ after
stepping. The engine leaves the joint intact — destroy the entity's
'Joint' (or lower the thresholds) yourself if it should break. Events
whose joints were destroyed since the step are dropped.
-}
newtype JointEvents = JointEvents [Entity]
  deriving (Show)

instance Component JointEvents where
  type Storage JointEvents = B3Space JointEvents

instance (MonadIO m, Has w m Physics) => Has w m JointEvents where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space JointEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.jointEvents sp.world
    joints <- readIORef sp.joints
    JointEvents <$> collectEvents (\ev -> jointEntityIn joints ev.jointId) evs

{- | A body that moved during the last 'Apecs.Box3D.Space.stepPhysics': its entity, its new
transform, and whether it fell asleep on this step (sleeping bodies stop
emitting moves — use the flag for a final render sync).
-}
data BodyMove = BodyMove
  { body :: !Entity
  , position :: !WVec
  , rotation :: !Quat
  , fellAsleep :: !Bool
  }
  deriving (Eq, Show)

{- | The bodies that moved during the last 'Apecs.Box3D.Space.stepPhysics', a read-only
global: @Moved moves <- get global@ after stepping. Iterating this
instead of every 'Apecs.Box3D.Body.Position' makes render sync O(moved) instead of
O(bodies): sleeping and static bodies don't appear. Box3D generates move
events unconditionally — there is no per-body opt-in flag, unlike
'Collisions'\/'Impacts'\/'SensorEvents', which need contact\/hit\/sensor
events enabled per shape. Events whose bodies were destroyed since the
step are dropped.
-}
newtype Moved = Moved [BodyMove]
  deriving (Show)

instance Component Moved where
  type Storage Moved = B3Space Moved

instance (MonadIO m, Has w m Physics) => Has w m Moved where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Moved) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B3Events.bodyMoveEvents sp.world
    bodies <- readIORef sp.bodies
    Moved
      <$> collectEvents
        ( \ev -> do
            let Transform pos rot = ev.transform
            fmap
              ( \ety ->
                  BodyMove
                    { body = ety
                    , position = pos
                    , rotation = rot
                    , fellAsleep = toBool ev.fellAsleep
                    }
              )
              <$> bodyEntityIn bodies ev.bodyId
        )
        evs
