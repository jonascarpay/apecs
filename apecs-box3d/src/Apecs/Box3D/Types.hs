{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

{-| The store shared by every component in "Apecs.Box3D" ('B3Space'),
the component values its registries embed ('Shape', 'Joint'), and the
resolution of engine objects back to their entities.
-}
module Apecs.Box3D.Types where

import Apecs
import Apecs.Core
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U

import Box3D.Body qualified as B3Body
import Box3D.Id (BodyId, JointId, ShapeId, WorldId)
import Box3D.Joint qualified as B3Joint
import Box3D.Shape qualified as B3Shape
import Box3D.UserData (getUserIndex)
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry

-- | Uninhabited component; add it to your world to get a physics space.
data Physics

-- | The engine shape plus the exact 'Shape' value that created it.
data ShapeRecord = ShapeRecord !ShapeId !Shape

{- | Engine-side tuning set through the joint sub-components
('Apecs.Box3D.Joint.MotorSpeed', 'Apecs.Box3D.Joint.JointLimits', ...), remembered alongside the
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

{- | The store shared by 'Physics' and all its sub-components: the engine
world plus entity registries for bodies, shapes and joints.
-}
data B3Space c = B3Space
  { world :: !WorldId
  , bodyDef :: !B3Body.BodyDef
  , shapeDef :: !B3Shape.ShapeDef
  , bodies :: !(IORef (IntMap BodyId))
  , shapes :: !(IORef (IntMap ShapeRecord))
  , joints :: !(IORef (IntMap JointRecord))
  , shapesByBody :: !(IORef (IntMap IS.IntSet))
  , jointsByBody :: !(IORef (IntMap IS.IntSet))
  , substeps :: !(IORef Int)
  }

cast :: B3Space a -> B3Space b
cast (B3Space w bd sd b s j sb jb i) = B3Space w bd sd b s j sb jb i

type instance Elem (B3Space c) = c

instance Component Physics where
  type Storage Physics = B3Space Physics

{- | Build the 'Physics' store around an existing engine world — one
made with a hand-tuned def (see 'initPhysicsWith'). The store takes
ownership: 'Apecs.Box3D.Space.destroyPhysics' destroys the world. The registries start
empty, so bodies already living in an adopted world are invisible to
the component layer (their user indices resolve to no registered
records, the usual raw-API caveat) — the layer's own spawns work
normally alongside them.
-}
initPhysicsFrom :: WorldId -> IO (B3Space c)
initPhysicsFrom w = do
  sd <- B3Shape.defaultShapeDef
  B3Space w
    <$> B3Body.defaultBodyDef
    -- Box3D defaults contact, hit and sensor event flags off; opt every
    -- layer-created shape in so 'Apecs.Box3D.Collision.Collisions', 'Apecs.Box3D.Collision.Impacts' and
    -- 'Apecs.Box3D.Collision.SensorEvents' have something to read (a shape both generates
    -- sensor events when it is itself a sensor and is visible to other
    -- sensors when it is a visitor).
    <*> pure
      sd
        { B3Shape.enableContactEvents = 1
        , B3Shape.enableHitEvents = 1
        , B3Shape.enableSensorEvents = 1
        }
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef 4

{- | Build the 'Physics' store around a caller-supplied
'B3World.WorldDef' — task system, capacities, bounds and the other
world-creation knobs 'explInit' fills with defaults. Hand-build the
world value in place of the generated @initWorld@:

@
customWorld = World \<$\> initPhysicsWith myWorldDef \<*\> explInit
@
-}
initPhysicsWith :: B3World.WorldDef -> IO (B3Space c)
initPhysicsWith wd = B3World.create wd >>= initPhysicsFrom

instance (MonadIO m) => ExplInit m (B3Space Physics) where
  explInit = liftIO $ B3World.defaultWorldDef >>= initPhysicsWith

-- Registries ----------------------------------------------------------------

{- | Add a dependent entity under a body entity in a reverse index
('shapesByBody', 'jointsByBody') — kept in step with the registries so
a 'Apecs.Box3D.Body.Body' destroy drops exactly its dependents' records instead of
filtering whole registries.
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
    Nothing -> error ("Entity " <> show ety <> " has no Box3D " <> what)

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

withBody :: B3Space c -> Int -> (BodyId -> IO a) -> IO a
withBody sp = withReg "Body" sp.bodies

overBody :: B3Space c -> Int -> (BodyId -> IO ()) -> IO ()
overBody sp = overReg sp.bodies

bodyExists :: (MonadIO m) => B3Space c -> Int -> m Bool
bodyExists sp = regExists sp.bodies

bodyMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
bodyMembers sp = regMembers sp.bodies

withShape :: B3Space c -> Int -> (ShapeId -> IO a) -> IO a
withShape sp ety f = withReg "Shape" sp.shapes ety (\(ShapeRecord s _) -> f s)

overShape :: B3Space c -> Int -> (ShapeId -> IO ()) -> IO ()
overShape sp ety f = overReg sp.shapes ety (\(ShapeRecord s _) -> f s)

shapeExists :: (MonadIO m) => B3Space c -> Int -> m Bool
shapeExists sp = regExists sp.shapes

shapeMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
shapeMembers sp = regMembers sp.shapes

withJoint :: B3Space c -> Int -> (JointId -> IO a) -> IO a
withJoint sp ety f = withReg "Joint" sp.joints ety (\(JointRecord j _ _) -> f j)

{- | Like 'withJoint', additionally handing the joint kind derived from
the stored spec ('jointSpecKind') — no engine round-trip.
-}
withKindedJoint :: B3Space c -> Int -> (B3Joint.JointType -> JointId -> IO a) -> IO a
withKindedJoint sp ety f =
  withReg "Joint" sp.joints ety (\(JointRecord j (Joint _ _ spec) _) -> f (jointSpecKind spec) j)

jointExists :: (MonadIO m) => B3Space c -> Int -> m Bool
jointExists sp = regExists sp.joints

jointMembers :: (MonadIO m) => B3Space c -> m (U.Vector Int)
jointMembers sp = regMembers sp.joints

-- | Whether an entity has a 'Joint' whose engine type is one of the given kinds.
jointIsKind :: B3Space c -> Int -> [B3Joint.JointType] -> IO Bool
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
jointKindMembers :: (MonadIO m) => B3Space c -> [B3Joint.JointType] -> m (U.Vector Int)
jointKindMembers sp kinds = liftIO $ do
  m <- readIORef sp.joints
  pure $
    U.fromList
      [ ety
      | (ety, JointRecord _ (Joint _ _ spec) _) <- IM.toList m
      , jointSpecKind spec `elem` kinds
      ]

{- | Gives an entity a collision shape attached to the 'Apecs.Box3D.Body.Body' of the given
entity (which may be the same entity). Carries the sub-components
'Apecs.Box3D.Shape.Density', 'Apecs.Box3D.Shape.Friction' and 'Apecs.Box3D.Shape.Elasticity'; re-setting the geometry
preserves them. Reads return the exact value written; geometry mutated
through the raw engine is not reflected.
-}
data Shape = Shape Entity Geometry
  deriving (Eq, Show)

{- | A joint between two bodies, specified in world space at creation
time. Joint frames are derived from the given world points with zero
reference rotation, except for the hinge, prismatic and wheel variants,
whose frames are additionally aligned to the given world axis or axes.
-}
data JointSpec
  = {- | A spherical (ball-socket) joint: the bodies pivot around a
    shared world point.
    -}
    PivotJoint WVec
  | -- | Keeps the two world anchor points at their current distance.
    DistanceJoint WVec WVec
  | -- | Rigidly welds the bodies together at a world point.
    WeldJoint WVec
  | {- | A revolute (hinge) joint: the bodies rotate relative to each
    other about a shared world point, constrained to a world-space
    axis.
    -}
    HingeJoint WVec WVec
  | {- | A hinge with an angular spring back to the creation orientation:
    stiffness in Hertz and a damping ratio.
    -}
    HingeSpringJoint WVec WVec Float Float
  | -- | A hinge with the relative angle limited to (lower, upper) radians.
    HingeLimitJoint WVec WVec Float Float
  | {- | A motorised hinge driving the relative angle at a speed (radians
    per second) with a maximum torque.
    -}
    HingeMotorJoint WVec WVec Float Float
  | {- | A prismatic (slider) joint: the bodies translate relative to
    each other along a world-space axis through the anchor, free
    between (lower, upper) meters.
    -}
    PrismaticJoint WVec WVec Float Float
  | {- | A prismatic (slider) joint with a damped spring back to the
    creation translation: stiffness in Hertz and a damping ratio.
    -}
    PrismaticSpringJoint WVec WVec Float Float
  | {- | A motorised prismatic (slider) joint driving the translation at
    a speed (meters per second) with a maximum force.
    -}
    PrismaticMotorJoint WVec WVec Float Float
  | {- | A wheel joint: entity A is the chassis and entity B the wheel.
    The wheel spins about the axle axis and the suspension lets it
    translate along the suspension axis through the anchor; the
    suspension spring is enabled with the given stiffness (Hertz) and
    damping ratio, and the spin motor and steering are left at engine
    defaults.
    -}
    WheelJoint WVec WVec WVec Float Float
  deriving (Eq, Show)

{- | The engine joint type a 'JointSpec' creates. The stored spec fully
determines it, so kind checks and tuning dispatch need no engine
round-trip.
-}
jointSpecKind :: JointSpec -> B3Joint.JointType
jointSpecKind spec = case spec of
  PivotJoint{} -> B3Joint.SphericalJoint
  DistanceJoint{} -> B3Joint.DistanceJoint
  WeldJoint{} -> B3Joint.WeldJoint
  HingeJoint{} -> B3Joint.RevoluteJoint
  HingeSpringJoint{} -> B3Joint.RevoluteJoint
  HingeLimitJoint{} -> B3Joint.RevoluteJoint
  HingeMotorJoint{} -> B3Joint.RevoluteJoint
  PrismaticJoint{} -> B3Joint.PrismaticJoint
  PrismaticSpringJoint{} -> B3Joint.PrismaticJoint
  PrismaticMotorJoint{} -> B3Joint.PrismaticJoint
  WheelJoint{} -> B3Joint.WheelJoint

{- | Gives an entity a joint connecting the 'Apecs.Box3D.Body.Body's of the two given
entities, which must be distinct (the engine rejects self-joints;
setting one is a silent no-op). Reads return the exact value written.
Tuning sub-component values ('Apecs.Box3D.Joint.MotorSpeed', 'Apecs.Box3D.Joint.JointLimits', ...)
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

{- | The shape and body entities behind an engine shape, if it is still
alive and registered (event buffers can reference shapes destroyed
after the step).
-}
shapeEntitiesIn :: IntMap ShapeRecord -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntitiesIn shapes =
  resolveReg
    B3Shape.isValid
    getUserIndex
    shapes
    (\s (ShapeRecord reg _) -> reg == s)
    (\ix (ShapeRecord _ (Shape bodyEty _)) -> (Entity ix, bodyEty))

-- | 'shapeEntitiesIn' over the live registry, for one-off resolutions.
shapeEntities :: B3Space c -> ShapeId -> IO (Maybe (Entity, Entity))
shapeEntities sp s = readIORef sp.shapes >>= (`shapeEntitiesIn` s)

{- | The joint entity behind an engine joint, if it is still alive and
registered (event buffers can reference joints destroyed after the
step).
-}
jointEntityIn :: IntMap JointRecord -> JointId -> IO (Maybe Entity)
jointEntityIn joints =
  resolveReg
    B3Joint.isValid
    getUserIndex
    joints
    (\j (JointRecord j' _ _) -> j' == j)
    (\ix _ -> Entity ix)

-- | 'jointEntityIn' over the live registry, for one-off resolutions.
jointEntity :: B3Space c -> JointId -> IO (Maybe Entity)
jointEntity sp j = readIORef sp.joints >>= (`jointEntityIn` j)

{- | The entity behind an engine body id, if it is still alive and
registered (event buffers can reference bodies destroyed after the
step).
-}
bodyEntityIn :: IntMap BodyId -> BodyId -> IO (Maybe Entity)
bodyEntityIn bodies =
  resolveReg
    B3Body.isValid
    getUserIndex
    bodies
    (\b reg -> reg == b)
    (\ix _ -> Entity ix)

-- | 'bodyEntityIn' over the live registry, for one-off resolutions.
bodyEntity :: B3Space c -> BodyId -> IO (Maybe Entity)
bodyEntity sp b = readIORef sp.bodies >>= (`bodyEntityIn` b)

{- | Resolve every event in a storable buffer, dropping the events whose
participants died since the step.
-}
collectEvents :: (VS.Storable e) => (e -> IO (Maybe a)) -> VS.Vector e -> IO [a]
collectEvents f = VS.foldr (\ev acc -> maybe id (:) <$> f ev <*> acc) (pure [])
