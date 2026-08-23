{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Collision, impact, sensor, joint and body-move events from the
last 'Apecs.Box2D.Space.stepPhysics', read as global components.
-}
module Apecs.Box2D.Collision where

import Apecs
import Apecs.Core
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Vector.Storable qualified as VS
import Foreign.Marshal.Utils (toBool)

import Box2D.Body qualified as B2Body
import Box2D.Collision qualified
import Box2D.Contact qualified as B2Contact
import Box2D.Events qualified as B2Events
import Box2D.Id (BodyId, ShapeId)
import Box2D.MathFunctions (rotGetAngle)
import Box2D.MathTypes (Transform (..), vec2Add)
import Box2D.Shape qualified as B2Shape
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

{- | A contact manifold: the surface normal and the world contact
points. Box2D uses speculative contacts, so a begin-touch manifold can
contain slightly separated points (positive separation) and can even
momentarily have no points.
-}
data ContactManifold = ContactManifold
  { normal :: !WVec
  -- ^ Contact normal, pointing from A to B.
  , points :: ![WVec]
  -- ^ World contact points, up to 2 in 2D.
  }
  deriving (Eq, Show)

{- | A contact from the last 'Apecs.Box2D.Space.stepPhysics': the shapes involved, the
bodies they hang off and — for begin-touch events — the contact
manifold. 'CollisionsEnd' events never carry a manifold; a begin-touch
event lacks one only when its contact died between the step and the
read (a shape destroyed after the step).

Equality compares the participants only, ignoring the manifold, so a
begin-touch value and the end-touch value of the same contact compare
equal — active-contact bookkeeping can pair them up with e.g.
'Data.List.delete'.
-}
data Collision = Collision
  { bodyA :: !Entity
  , shapeA :: !Entity
  , bodyB :: !Entity
  , shapeB :: !Entity
  , manifold :: !(Maybe ContactManifold)
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
resolvePair :: ShapeRegs -> ShapeId -> ShapeId -> IO (Maybe ((Entity, Entity), (Entity, Entity)))
resolvePair regs sA sB = do
  ma <- shapeEntitiesIn regs sA
  mb <- shapeEntitiesIn regs sB
  pure ((,) <$> ma <*> mb)

{- | The shape/body entities behind a contact's two shape ids, if both
are still alive and registered; no manifold.
-}
toCollision :: ShapeRegs -> ShapeId -> ShapeId -> IO (Maybe Collision)
toCollision regs sA sB =
  fmap (\((sa, ba), (sb, bb)) -> Collision ba sa bb sb Nothing)
    <$> resolvePair regs sA sB

{- | 'toCollision' for a begin-touch event, with the contact manifold
filled in: the normal plus the world position of each manifold point
(body A's world center of mass plus the point's A-side anchor). If the
contact is no longer valid (a shape was destroyed after the step) the
manifold stays 'Nothing'.
-}
toBeginCollision :: ShapeRegs -> IntMap BodyId -> B2World.ContactBeginTouchEvent -> IO (Maybe Collision)
toBeginCollision regs bodies ev = do
  let contact = ev.contactId
  valid <- B2Contact.isValid contact
  if not valid then
    toCollision regs ev.shapeIdA ev.shapeIdB
  else do
    cd <- B2Contact.getData contact
    -- resolve from the ContactData's own shape order, so the A/B
    -- entities stay consistent with the manifold normal's A-to-B
    -- orientation
    mc <- toCollision regs cd.shapeIdA cd.shapeIdB
    forM mc $ \c -> do
      let
        m = cd.manifold
        anchors = m.points
      pts <-
        if VS.null anchors then
          pure []
        else do
          -- body A's world center of mass turns the manifold's A-side
          -- anchors into world points; its id comes from the body
          -- registry (toCollision just resolved the entity) rather
          -- than a getBody FFI round-trip
          let Entity bIx = c.bodyA
          bodyAId <- maybe (B2Shape.getBody cd.shapeIdA) pure (IM.lookup bIx bodies)
          comA <- B2Body.getWorldCenter bodyAId
          pure (map (vec2Add comA . (.anchorA)) (VS.toList anchors))
      pure c{manifold = Just (ContactManifold m.normal pts)}

{- | The begin-touch contacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @Collisions touches <- get global@ after stepping. Shapes
created by this layer opt into contact events; events whose shapes
were destroyed since the step are dropped.
-}
newtype Collisions = Collisions [Collision]
  deriving (Show)

instance Component Collisions where
  type Storage Collisions = B2Space Collisions

instance (MonadIO m, Has w m Physics) => Has w m Collisions where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Collisions) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactBeginTouchEvents sp.world
    regs <- shapeRegs sp
    bodies <- readIORef sp.bodies
    Collisions <$> collectEvents (toBeginCollision regs bodies) evs

{- | The end-touch contacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @CollisionsEnd separations <- get global@ after stepping — the
counterpart of 'Collisions' for contacts that stopped touching. Events
whose shapes were destroyed since the step are dropped; this bites
harder here than for begin-touch, since destroying a shape mid-contact
drops its end event — clean up any per-contact bookkeeping when
destroying shapes. End events carry no manifold ('manifold'
is 'Nothing').
-}
newtype CollisionsEnd = CollisionsEnd [Collision]
  deriving (Show)

instance Component CollisionsEnd where
  type Storage CollisionsEnd = B2Space CollisionsEnd

instance (MonadIO m, Has w m Physics) => Has w m CollisionsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollisionsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactEndTouchEvents sp.world
    regs <- shapeRegs sp
    CollisionsEnd <$> collectEvents (\ev -> toCollision regs ev.shapeIdA ev.shapeIdB) evs

{- | An above-threshold impact from the last 'Apecs.Box2D.Space.stepPhysics': the entities
involved, the world-space contact point, the contact normal (pointing
from A to B) and the approach speed. Only generated when the approach
speed exceeds the world's hit-event threshold (engine default 1;
tune with 'Apecs.Box2D.Space.HitEventThreshold').
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

{- | The impacts of the last 'Apecs.Box2D.Space.stepPhysics', a read-only global:
@Impacts hits <- get global@ after stepping.
-}
newtype Impacts = Impacts [Impact]
  deriving (Show)

instance Component Impacts where
  type Storage Impacts = B2Space Impacts

instance (MonadIO m, Has w m Physics) => Has w m Impacts where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Impacts) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.contactHitEvents sp.world
    regs <- shapeRegs sp
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
              <$> resolvePair regs ev.shapeIdA ev.shapeIdB
        )
        evs

{- | A sensor overlap that began or ended during the last 'Apecs.Box2D.Space.stepPhysics':
the 'Apecs.Box2D.Shape.Sensor' shape (and the body it hangs off) and the visitor shape
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
toSensorEvent :: ShapeRegs -> ShapeId -> ShapeId -> IO (Maybe SensorEvent)
toSensorEvent regs sensorS visitorS =
  fmap (\((sShape, sBody), (vShape, vBody)) -> SensorEvent sBody sShape vBody vShape)
    <$> resolvePair regs sensorS visitorS

{- | The sensor overlaps that began during the last 'Apecs.Box2D.Space.stepPhysics', a
read-only global: @SensorEvents begins <- get global@ after stepping —
the sensor counterpart of 'Collisions', with 'SensorEventsEnd' as the
ending side. Shapes created by this layer opt into sensor events, both
as sensors and as visitors; events whose sensor or visitor shape was
destroyed since the step are dropped.
-}
newtype SensorEvents = SensorEvents [SensorEvent]
  deriving (Show)

instance Component SensorEvents where
  type Storage SensorEvents = B2Space SensorEvents

instance (MonadIO m, Has w m Physics) => Has w m SensorEvents where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SensorEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.sensorBeginTouchEvents sp.world
    regs <- shapeRegs sp
    SensorEvents <$> collectEvents (\ev -> toSensorEvent regs ev.sensorShapeId ev.visitorShapeId) evs

{- | The sensor overlaps that ended during the last 'Apecs.Box2D.Space.stepPhysics', the
counterpart of 'SensorEvents' — split so a reader pays only for the
buffer it consumes. Destroying a shape mid-overlap drops its end event,
same caveat as 'CollisionsEnd'.
-}
newtype SensorEventsEnd = SensorEventsEnd [SensorEvent]
  deriving (Show)

instance Component SensorEventsEnd where
  type Storage SensorEventsEnd = B2Space SensorEventsEnd

instance (MonadIO m, Has w m Physics) => Has w m SensorEventsEnd where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SensorEventsEnd) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.sensorEndTouchEvents sp.world
    regs <- shapeRegs sp
    SensorEventsEnd <$> collectEvents (\ev -> toSensorEvent regs ev.sensorShapeId ev.visitorShapeId) evs

{- | The joints whose force or torque threshold ('Apecs.Box2D.Joint.JointForceThreshold',
'Apecs.Box2D.Joint.JointTorqueThreshold') was exceeded during the last 'Apecs.Box2D.Space.stepPhysics', a
read-only global: @JointEvents overloaded <- get global@ after
stepping. The engine leaves the joint intact — destroy the entity's
'Joint' (or lower the thresholds) yourself if it should break. Events
whose joints were destroyed since the step are dropped.
-}
newtype JointEvents = JointEvents [Entity]
  deriving (Show)

instance Component JointEvents where
  type Storage JointEvents = B2Space JointEvents

instance (MonadIO m, Has w m Physics) => Has w m JointEvents where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space JointEvents) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.jointEvents sp.world
    joints <- readIORef sp.joints
    JointEvents <$> collectEvents (\ev -> jointEntityIn joints ev.jointId) evs

{- | A body that moved during the last 'Apecs.Box2D.Space.stepPhysics': its entity, its new
transform, and whether it fell asleep on this step (sleeping bodies stop
emitting moves — use the flag for a final render sync).
-}
data BodyMove = BodyMove
  { body :: !Entity
  , position :: !WVec
  , angle :: !Float
  , fellAsleep :: !Bool
  }
  deriving (Eq, Show)

{- | The bodies that moved during the last 'Apecs.Box2D.Space.stepPhysics', a read-only
global: @Moved moves <- get global@ after stepping. Iterating this
instead of every 'Apecs.Box2D.Body.Position' makes render sync O(moved) instead of
O(bodies): sleeping and static bodies don't appear. Box2D generates move
events unconditionally — there is no per-body opt-in flag, unlike
'Collisions'\/'Impacts'\/'SensorEvents', which need contact\/hit\/sensor
events enabled per shape. Events whose bodies were destroyed since the
step are dropped.
-}
newtype Moved = Moved [BodyMove]
  deriving (Show)

instance Component Moved where
  type Storage Moved = B2Space Moved

instance (MonadIO m, Has w m Physics) => Has w m Moved where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Moved) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ do
    evs <- B2Events.bodyMoveEvents sp.world
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
                    , angle = rotGetAngle rot
                    , fellAsleep = toBool ev.fellAsleep
                    }
              )
              <$> bodyEntityIn bodies ev.bodyId
        )
        evs
