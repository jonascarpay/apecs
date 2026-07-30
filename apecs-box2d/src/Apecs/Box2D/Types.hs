{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

{-| The store shared by every component in "Apecs.Box2D" ('B2Space'),
the component values its registries embed ('Shape', 'Chain', 'Joint'),
and the resolution of engine objects back to their entities.
-}
module Apecs.Box2D.Types where

import Apecs
import Apecs.Core
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U

import Box2D.Body qualified as B2Body
import Box2D.Id (BodyId, ChainId, JointId, ShapeId (..), WorldId)
import Box2D.Joint qualified as B2Joint
import Box2D.MathTypes (Vec2)
import Box2D.Shape qualified as B2Shape
import Box2D.UserData (getUserIndex)
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry

-- | Uninhabited component; add it to your world to get a physics space.
data Physics

-- | The engine shape plus the exact 'Shape' value that created it.
data ShapeRecord = ShapeRecord !ShapeId !Shape

{- | Engine-side tuning set through the joint sub-components
('Apecs.Box2D.Joint.MotorSpeed', 'Apecs.Box2D.Joint.JointLimits', ...), remembered alongside the
stored 'Joint' so a 'Joint' re-set can re-apply it to the recreated
engine joint. A field is 'Nothing' until its component is set.
-}
data JointTuning = JointTuning
  { motorSpeed :: !(Maybe Float)
  , motorMaxTorque :: !(Maybe Float)
  , motorMaxForce :: !(Maybe Float)
  , limits :: !(Maybe (Float, Float))
  , collideConnected :: !(Maybe Bool)
  , forceThreshold :: !(Maybe Float)
  , torqueThreshold :: !(Maybe Float)
  }

noTuning :: JointTuning
noTuning = JointTuning Nothing Nothing Nothing Nothing Nothing Nothing Nothing

{- | The engine joint, the exact 'Joint' value that created it, and the
tuning applied through the sub-components since.
-}
data JointRecord = JointRecord !JointId !Joint !JointTuning

{- | The engine chain, the packed ids of the segment shapes the engine
generated for it (the full 'ShapeId' words, so stale entries can never
match a live shape that reuses an index slot), and the exact 'Chain'
value that created it.
-}
data ChainRecord = ChainRecord !ChainId !IS.IntSet !Chain

{- | The store shared by 'Physics' and all its sub-components: the engine
world plus entity registries for bodies, shapes, joints and chains.
-}
data B2Space c = B2Space
  { world :: !WorldId
  , bodyDef :: !B2Body.BodyDef
  , shapeDef :: !B2Shape.ShapeDef
  , bodies :: !(IORef (IntMap BodyId))
  , shapes :: !(IORef (IntMap ShapeRecord))
  , joints :: !(IORef (IntMap JointRecord))
  , chains :: !(IORef (IntMap ChainRecord))
  , shapesByBody :: !(IORef (IntMap IS.IntSet))
  , jointsByBody :: !(IORef (IntMap IS.IntSet))
  , chainsByBody :: !(IORef (IntMap IS.IntSet))
  , substeps :: !(IORef Int)
  }

cast :: B2Space a -> B2Space b
cast (B2Space w bd sd b s j c sb jb cb i) = B2Space w bd sd b s j c sb jb cb i

type instance Elem (B2Space c) = c

instance Component Physics where
  type Storage Physics = B2Space Physics

{- | Build the 'Physics' store around an existing engine world — one
made with a hand-tuned def (see 'initPhysicsWith') or restored with
@b2World_CreateFromSnapshot@. The store takes ownership:
'Apecs.Box2D.Space.destroyPhysics' destroys the world. The registries start empty, so
bodies already living in an adopted world are invisible to the
component layer (their user indices resolve to no registered records,
the usual raw-API caveat) — the layer's own spawns work normally
alongside them.
-}
initPhysicsFrom :: WorldId -> IO (B2Space c)
initPhysicsFrom w = do
  sd <- B2Shape.defaultShapeDef
  B2Space w
    <$> B2Body.defaultBodyDef
    -- Box2D defaults contact, hit and sensor event flags off; opt every
    -- layer-created shape in so 'Apecs.Box2D.Collision.Collisions', 'Apecs.Box2D.Collision.Impacts' and
    -- 'Apecs.Box2D.Collision.SensorEvents' have something to read (a shape both generates
    -- sensor events when it is itself a sensor and is visible to other
    -- sensors when it is a visitor).
    <*> pure
      sd
        { B2Shape.enableContactEvents = 1
        , B2Shape.enableHitEvents = 1
        , B2Shape.enableSensorEvents = 1
        }
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef 4

{- | Build the 'Physics' store around a caller-supplied
'B2World.WorldDef' — task system, capacities, bounds and the other
world-creation knobs 'explInit' fills with defaults. Hand-build the
world value in place of the generated @initWorld@:

@
customWorld = World \<$\> initPhysicsWith myWorldDef \<*\> explInit
@
-}
initPhysicsWith :: B2World.WorldDef -> IO (B2Space c)
initPhysicsWith wd = B2World.create wd >>= initPhysicsFrom

instance (MonadIO m) => ExplInit m (B2Space Physics) where
  explInit = liftIO $ B2World.defaultWorldDef >>= initPhysicsWith

-- Registries ----------------------------------------------------------------

{- | Add a dependent entity under a body entity in a reverse index
('shapesByBody', 'jointsByBody', 'chainsByBody') — kept in step with
the registries so a 'Apecs.Box2D.Body.Body' destroy drops exactly its dependents'
records instead of filtering whole registries.
-}
depInsert :: IORef (IntMap IS.IntSet) -> Int -> Int -> IO ()
depInsert ref bodyEty ety = modifyIORef' ref (IM.insertWith IS.union bodyEty (IS.singleton ety))

-- | Drop a dependent entity from a body entity's reverse-index set.
depDelete :: IORef (IntMap IS.IntSet) -> Int -> Int -> IO ()
depDelete ref bodyEty ety = modifyIORef' ref (IM.update trim bodyEty)
  where
    trim s =
      let s' = IS.delete ety s
      in if IS.null s' then Nothing else Just s'

{- | Look up an entity's engine object. Only safe under the apecs 'ExplGet'
contract: the caller has checked existence.
-}
withReg :: String -> IORef (IntMap v) -> Int -> (v -> IO a) -> IO a
withReg what ref ety f = do
  m <- readIORef ref
  case IM.lookup ety m of
    Just v -> f v
    Nothing -> error ("Entity " <> show ety <> " has no Box2D " <> what)

{- | Run an action over an entity's engine object, or do nothing when the
entity has none — setter semantics, matching apecs-physics.
-}
overReg :: IORef (IntMap v) -> Int -> (v -> IO ()) -> IO ()
overReg ref ety f = readIORef ref >>= mapM_ f . IM.lookup ety

regExists :: (MonadIO m) => IORef (IntMap v) -> Int -> m Bool
regExists ref ety = liftIO $ IM.member ety <$> readIORef ref

regMembers :: (MonadIO m) => IORef (IntMap v) -> m (U.Vector Int)
regMembers ref = liftIO $ do
  m <- readIORef ref
  pure (U.fromListN (IM.size m) (IM.keys m))

withBody :: B2Space c -> Int -> (BodyId -> IO a) -> IO a
withBody sp = withReg "Body" sp.bodies

overBody :: B2Space c -> Int -> (BodyId -> IO ()) -> IO ()
overBody sp = overReg sp.bodies

bodyExists :: (MonadIO m) => B2Space c -> Int -> m Bool
bodyExists sp = regExists sp.bodies

bodyMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
bodyMembers sp = regMembers sp.bodies

withShape :: B2Space c -> Int -> (ShapeId -> IO a) -> IO a
withShape sp ety f = withReg "Shape" sp.shapes ety (\(ShapeRecord s _) -> f s)

overShape :: B2Space c -> Int -> (ShapeId -> IO ()) -> IO ()
overShape sp ety f = overReg sp.shapes ety (\(ShapeRecord s _) -> f s)

shapeExists :: (MonadIO m) => B2Space c -> Int -> m Bool
shapeExists sp = regExists sp.shapes

shapeMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
shapeMembers sp = regMembers sp.shapes

withJoint :: B2Space c -> Int -> (JointId -> IO a) -> IO a
withJoint sp ety f = withReg "Joint" sp.joints ety (\(JointRecord j _ _) -> f j)

{- | Like 'withJoint', additionally handing the joint kind derived from
the stored spec ('jointSpecKind') — no engine round-trip.
-}
withKindedJoint :: B2Space c -> Int -> (B2Joint.JointType -> JointId -> IO a) -> IO a
withKindedJoint sp ety f =
  withReg "Joint" sp.joints ety (\(JointRecord j (Joint _ _ spec) _) -> f (jointSpecKind spec) j)

jointExists :: (MonadIO m) => B2Space c -> Int -> m Bool
jointExists sp = regExists sp.joints

jointMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
jointMembers sp = regMembers sp.joints

withChain :: B2Space c -> Int -> (ChainId -> IO a) -> IO a
withChain sp ety f = withReg "Chain" sp.chains ety (\(ChainRecord c _ _) -> f c)

chainExists :: (MonadIO m) => B2Space c -> Int -> m Bool
chainExists sp = regExists sp.chains

chainMembers :: (MonadIO m) => B2Space c -> m (U.Vector Int)
chainMembers sp = regMembers sp.chains

-- | Whether an entity has a 'Joint' whose engine type is one of the given kinds.
jointIsKind :: B2Space c -> Int -> [B2Joint.JointType] -> IO Bool
jointIsKind sp ety kinds = do
  m <- readIORef sp.joints
  pure $ case IM.lookup ety m of
    Nothing -> False
    Just (JointRecord _ (Joint _ _ spec) _) -> jointSpecKind spec `elem` kinds

{- | The entities whose 'Joint' engine type is one of the given kinds.
Kind-restricted components must keep their members consistent with
'jointIsKind' in @explExists@: @cmap@\/@cfold@ call @explGet@ on every
member without an existence check, and an unfiltered members list
would hand joints of the wrong kind to a type-specific engine getter.
-}
jointKindMembers :: (MonadIO m) => B2Space c -> [B2Joint.JointType] -> m (U.Vector Int)
jointKindMembers sp kinds = liftIO $ do
  m <- readIORef sp.joints
  pure $
    U.fromList
      [ ety
      | (ety, JointRecord _ (Joint _ _ spec) _) <- IM.toList m
      , jointSpecKind spec `elem` kinds
      ]

{- | Gives an entity a collision shape attached to the 'Apecs.Box2D.Body.Body' of the given
entity (which may be the same entity). Carries the sub-components
'Apecs.Box2D.Shape.Density', 'Apecs.Box2D.Shape.Friction' and 'Apecs.Box2D.Shape.Elasticity'; re-setting the geometry
preserves them. Reads return the exact value written; geometry mutated
through the raw engine is not reflected.
-}
data Shape = Shape Entity Geometry
  deriving (Eq, Show)

{- | Gives an entity a chain of connected line segments attached to the
'Apecs.Box2D.Body.Body' of the given entity — smooth static terrain outlines without the
ghost collisions of separate 'GeoSegment's. Points are in body-local
coordinates. Collision is one-sided: the solid face is to the right when
facing from one point towards the next, so for a loop a
counter-clockwise winding faces outward and a clockwise winding faces
inward (the same convention 'GeoPolygon' uses for its CCW hull). When the
loop flag is set the chain closes by connecting the last point back to
the first; either way at least 4 points are required, and setting fewer
raises an error in the style of 'GeoPolygon'. Chains are meant for
static bodies. Re-setting recreates the engine chain; reads return the
exact value written. Setting it on an entity whose body entity has no
'Apecs.Box2D.Body.Body' is a silent no-op.

The segments the engine creates for a chain are its own internal
@b2ChainSegment@ shapes; there is no matching 'Shape' component for
them, so they are not entered into this layer's shape registry. Instead,
each segment is stamped with the chain entity's user index directly (see
'Box2D.UserData.setUserIndex'), and the chain's contact and hit events
are switched on for every segment at creation — mirroring what 'explInit'
does per-shape for layer-created 'Shape's. Event and query resolution
falls back to a chain lookup keyed by that index when a shape isn't found
in the shape registry, so chain segments now surface in 'Apecs.Box2D.Collision.Collisions',
'Apecs.Box2D.Collision.CollisionsEnd' and 'Apecs.Box2D.Collision.Impacts', and are visible to the queries
('Apecs.Box2D.Query.segmentQuery', 'Apecs.Box2D.Query.segmentQueryAll', 'Apecs.Box2D.Query.aabbQuery', 'Apecs.Box2D.Query.pointQuery',
'Apecs.Box2D.Query.overlapQuery', 'Apecs.Box2D.Query.sweepQuery') instead of being dropped
('Apecs.Box2D.Query.containsPointQuery' is the exception: its exact refinement,
@b2Shape_TestPoint@, reports no containment for chain segments, same as
for plain 'GeoSegment' shapes, so chains never pass it)
— in particular 'Apecs.Box2D.Query.segmentQuery' no longer returns 'Nothing' just
because a chain segment is the closest hit. In every case the CHAIN
entity is reported in the shape slot, not a per-segment entity: a reader
following a 'Apecs.Box2D.Collision.Collision' or 'Apecs.Box2D.Query.RayHit' shape entity to a 'Shape'
component won't find one, but will find a 'Chain'. Chain creation also
turns on @chainDefEnableSensorEvents@, the chain-level counterpart of
'Shape'\'s 'Apecs.Box2D.Shape.Sensor' visitor opt-in, so chains are visible to sensors the
same way ordinary shapes are.
-}
data Chain = Chain Entity (VS.Vector Vec2) Bool
  deriving (Eq, Show)

{- | A joint between two bodies, specified in world space at creation
time. Joint frames are derived from the given world points with zero
reference rotation, except for the prismatic and wheel variants, whose
frames are additionally aligned to the given world axis.
-}
data JointSpec
  = -- | A revolute joint: the bodies rotate around a shared world point.
    PivotJoint WVec
  | -- | Keeps the two world anchor points at their current distance.
    DistanceJoint WVec WVec
  | -- | Rigidly welds the bodies together at a world point.
    WeldJoint WVec
  | {- | A damped spring between two world anchors, resting at their
    current distance: stiffness in Hertz and a damping ratio.
    -}
    SpringJoint WVec WVec Float Float
  | -- | The anchor distance moves freely between a minimum and maximum.
    SlideJoint WVec WVec Float Float
  | {- | A pivot with an angular spring back to the creation orientation:
    stiffness in Hertz and a damping ratio.
    -}
    RotarySpringJoint WVec Float Float
  | -- | A pivot with the relative angle limited to (lower, upper) radians.
    RotaryLimitJoint WVec Float Float
  | {- | A motorised pivot driving the relative angle at a speed (radians
    per second) with a maximum torque.
    -}
    RotaryMotorJoint WVec Float Float
  | {- | A prismatic joint: the bodies slide relative to each other along
    a world-space axis through the anchor, free between (lower, upper)
    meters from the anchor, with no relative rotation.
    -}
    PrismaticJoint WVec WVec Float Float
  | {- | A prismatic joint with a damped spring back to the creation
    translation: stiffness in Hertz and a damping ratio.
    -}
    PrismaticSpringJoint WVec WVec Float Float
  | {- | A motorised prismatic joint driving the translation at a speed
    (meters per second) with a maximum force.
    -}
    PrismaticMotorJoint WVec WVec Float Float
  | {- | A wheel joint: entity A is the chassis and entity B the wheel,
    which spins freely and rides the suspension spring along the axis
    through the anchor, at the given stiffness (Hertz) and damping
    ratio.
    -}
    WheelJoint WVec WVec Float Float
  | {- | Drives the relative velocity between the bodies at the anchor:
    desired linear velocity and its maximum force, then desired angular
    velocity and its maximum torque. With zero velocities it acts as
    top-down friction, damping relative motion without pinning the
    bodies to the anchor.
    -}
    MotorJoint WVec WVec Float Float Float
  deriving (Eq, Show)

{- | The engine joint type a 'JointSpec' creates. The stored spec fully
determines it, so kind checks and tuning dispatch need no engine
round-trip.
-}
jointSpecKind :: JointSpec -> B2Joint.JointType
jointSpecKind spec = case spec of
  PivotJoint{} -> B2Joint.RevoluteJoint
  RotarySpringJoint{} -> B2Joint.RevoluteJoint
  RotaryLimitJoint{} -> B2Joint.RevoluteJoint
  RotaryMotorJoint{} -> B2Joint.RevoluteJoint
  DistanceJoint{} -> B2Joint.DistanceJoint
  SpringJoint{} -> B2Joint.DistanceJoint
  SlideJoint{} -> B2Joint.DistanceJoint
  WeldJoint{} -> B2Joint.WeldJoint
  PrismaticJoint{} -> B2Joint.PrismaticJoint
  PrismaticSpringJoint{} -> B2Joint.PrismaticJoint
  PrismaticMotorJoint{} -> B2Joint.PrismaticJoint
  WheelJoint{} -> B2Joint.WheelJoint
  MotorJoint{} -> B2Joint.MotorJoint

{- | Gives an entity a joint connecting the 'Apecs.Box2D.Body.Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
Tuning sub-component values ('Apecs.Box2D.Joint.MotorSpeed', 'Apecs.Box2D.Joint.JointLimits', ...)
the current joint kind accepts are remembered for as long as the
'Joint' exists (sets the kind rejects are dropped). A re-set recreates
the engine joint from the new spec, then re-applies the remembered
tuning over it — overlapping spec parameters lose; a field the new
kind does not accept stays dormant until a re-set restores the kind,
returning in that kind's units.
-}
data Joint = Joint Entity Entity JointSpec
  deriving (Eq, Show)

{- | Resolve a live engine object back through the wrapper's user-index
channel against a registry snapshot: validity check, user index read,
lookup, match. Objects created through the raw engine API have no user
index and read back as 0, a legitimate entity — the match predicate
must identify the registered record as belonging to this very object,
which drops them.
-}
resolveReg
  :: (i -> IO Bool)
  -> (i -> IO Int)
  -> IM.IntMap r
  -> (i -> r -> Bool)
  -> (Int -> r -> a)
  -> i
  -> IO (Maybe a)
resolveReg isValid getIx m match mk x = do
  alive <- isValid x
  if not alive then
    pure Nothing
  else do
    ix <- getIx x
    pure $ case IM.lookup ix m of
      Just r | match x r -> Just (mk ix r)
      _ -> Nothing

{- | Both shape-resolution registries, snapshotted once per event-buffer
or query read instead of re-read per event.
-}
data ShapeRegs = ShapeRegs !(IntMap ShapeRecord) !(IntMap ChainRecord)

shapeRegs :: B2Space c -> IO ShapeRegs
shapeRegs sp = ShapeRegs <$> readIORef sp.shapes <*> readIORef sp.chains

{- | The shape and body entities behind an engine shape, if it is still
alive and registered (event buffers can reference shapes destroyed
after the step). The shape registry is the fast path; a shape not found
there (in particular, a chain's internal @b2ChainSegment@, which is
never entered into it) falls back to a lookup in the chain registry —
identity there is membership in the chain's recorded segment ids —
resolving to the CHAIN entity as the "shape" and its body entity; see
the 'Chain' haddock.
-}
shapeEntitiesIn :: ShapeRegs -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntitiesIn (ShapeRegs shapes chains) s@(ShapeId w) = do
  -- open-coded 'resolveReg' over both maps: one validity check and one
  -- user-index read serve the fast path and the chain fallback alike
  alive <- B2Shape.isValid s
  if not alive then
    pure Nothing
  else do
    ix <- getUserIndex s
    pure $ case IM.lookup ix shapes of
      Just (ShapeRecord reg (Shape bodyEty _)) | reg == s -> Just (Entity ix, bodyEty)
      _ -> case IM.lookup ix chains of
        Just (ChainRecord _ segSet (Chain bodyEty _ _))
          | IS.member (fromIntegral w) segSet -> Just (Entity ix, bodyEty)
        _ -> Nothing

-- | 'shapeEntitiesIn' over the live registries, for one-off resolutions.
shapeEntities :: B2Space c -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntities sp s = shapeRegs sp >>= (`shapeEntitiesIn` s)

{- | The joint entity behind an engine joint, if it is still alive and
registered (event buffers can reference joints destroyed after the
step).
-}
jointEntityIn :: IntMap JointRecord -> JointId -> IO (Maybe Entity)
jointEntityIn joints =
  resolveReg
    B2Joint.isValid
    getUserIndex
    joints
    (\j (JointRecord j' _ _) -> j' == j)
    (\ix _ -> Entity ix)

-- | 'jointEntityIn' over the live registry, for one-off resolutions.
jointEntity :: B2Space c -> JointId -> IO (Maybe Entity)
jointEntity sp j = readIORef sp.joints >>= (`jointEntityIn` j)

{- | The entity behind an engine body id, if it is still alive and
registered (event buffers can reference bodies destroyed after the
step).
-}
bodyEntityIn :: IntMap BodyId -> BodyId -> IO (Maybe Entity)
bodyEntityIn bodies =
  resolveReg
    B2Body.isValid
    getUserIndex
    bodies
    (\b reg -> reg == b)
    (\ix _ -> Entity ix)

-- | 'bodyEntityIn' over the live registry, for one-off resolutions.
bodyEntity :: B2Space c -> BodyId -> IO (Maybe Entity)
bodyEntity sp b = readIORef sp.bodies >>= (`bodyEntityIn` b)

{- | Resolve every event in a storable buffer, dropping the events whose
participants died since the step.
-}
collectEvents :: (VS.Storable e) => (e -> IO (Maybe a)) -> VS.Vector e -> IO [a]
collectEvents f = VS.foldr (\ev acc -> maybe id (:) <$> f ev <*> acc) (pure [])
