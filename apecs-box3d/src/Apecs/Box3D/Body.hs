{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| The 'Body' component and its sub-components, reading and writing
the engine body directly.
-}
module Apecs.Box3D.Body where

import Apecs
import Apecs.Core
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Utils (fromBool, toBool)

import Box3D.Body qualified as B3Body
import Box3D.Id (BodyId)
import Box3D.MathTypes (Matrix3 (..), Quat (..), Transform (..), Vec3 (..))
import Box3D.UserData (setUserIndex)

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

{- | Gives an entity a Box3D body. Deleting it also deletes the shapes
attached to it. A body carries the sub-components 'Position',
'Velocity', 'Rotation', 'AngularVelocity', 'BodyMass', 'Force' and
'Torque'; they exist as long as the entity has a @Body@, and setting
them on an entity without one does nothing.
-}
data Body = DynamicBody | KinematicBody | StaticBody
  deriving (Eq, Ord, Enum, Show)

toB3BodyType :: Body -> B3Body.BodyType
toB3BodyType DynamicBody = B3Body.DynamicBody
toB3BodyType KinematicBody = B3Body.KinematicBody
toB3BodyType StaticBody = B3Body.StaticBody

fromB3BodyType :: B3Body.BodyType -> Body
fromB3BodyType ty = case ty of
  B3Body.DynamicBody -> DynamicBody
  B3Body.KinematicBody -> KinematicBody
  B3Body.StaticBody -> StaticBody

instance Component Body where
  type Storage Body = B3Space Body

instance (MonadIO m, Has w m Physics) => Has w m Body where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Body) where
  explSet sp ety btype = liftIO $ do
    bodies <- readIORef sp.bodies
    case IM.lookup ety bodies of
      Just b -> B3Body.setType b (toB3BodyType btype)
      Nothing -> do
        b <- B3Body.create sp.world (sp.bodyDef){B3Body.type_ = toB3BodyType btype}
        setUserIndex b ety
        modifyIORef' sp.bodies (IM.insert ety b)

instance (MonadIO m) => ExplGet m (B3Space Body) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap fromB3BodyType . B3Body.getType

instance (MonadIO m) => ExplDestroy m (B3Space Body) where
  explDestroy sp ety = liftIO $ do
    bodies <- readIORef sp.bodies
    forM_ (IM.lookup ety bodies) $ \b -> do
      -- destroy the engine body first: the ShapeRecords keep mesh and
      -- height-field ForeignPtrs alive, and dropping them before the
      -- engine call would let a GC finalize geometry the engine shapes
      -- still reference
      B3Body.destroy b
      -- the engine destroys attached shapes and joints along with the
      -- body; the reverse indices name their entity records to drop,
      -- so a destroy costs O(dependents), not a scan of whole registries
      shapeDeps <- IM.findWithDefault IS.empty ety <$> readIORef sp.shapesByBody
      jointDeps <- IM.findWithDefault IS.empty ety <$> readIORef sp.jointsByBody
      modifyIORef' sp.shapes (`IM.withoutKeys` shapeDeps)
      modifyIORef' sp.shapesByBody (IM.delete ety)
      -- a joint hangs off two bodies: trim it from the counterpart
      -- body's index (this body's whole entry is dropped below)
      joints <- readIORef sp.joints
      forM_ (IS.toList jointDeps) $ \jEty ->
        forM_ (IM.lookup jEty joints) $ \(JointRecord _ (Joint (Entity a) (Entity b') _) _) ->
          depDelete sp.jointsByBody (if a == ety then b' else a) jEty
      modifyIORef' sp.jointsByBody (IM.delete ety)
      modifyIORef' sp.joints (`IM.withoutKeys` jointDeps)
      modifyIORef' sp.bodies (IM.delete ety)

instance (MonadIO m) => ExplMembers m (B3Space Body) where
  explMembers = bodyMembers

-- | The raw Box3D body of an entity, for use with "Box3D.Body" directly.
newtype B3BodyId = B3BodyId BodyId
  deriving (Eq, Show)

instance Component B3BodyId where
  type Storage B3BodyId = B3Space B3BodyId

instance (MonadIO m, Has w m Physics) => Has w m B3BodyId where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space B3BodyId) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety (pure . B3BodyId)

instance (MonadIO m) => ExplMembers m (B3Space B3BodyId) where
  explMembers = bodyMembers

-- Body sub-components ------------------------------------------------------

-- | Where a 'Body' is, in world coordinates.
newtype Position = Position WVec
  deriving (Eq, Show)

instance Component Position where
  type Storage Position = B3Space Position

instance (MonadIO m, Has w m Physics) => Has w m Position where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Position) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Position . B3Body.getPosition

instance (MonadIO m) => ExplSet m (B3Space Position) where
  explSet sp ety (Position p) = liftIO $
    overBody sp ety $ \b -> do
      rot <- B3Body.getRotation b
      B3Body.setTransform b p rot

instance (MonadIO m) => ExplMembers m (B3Space Position) where
  explMembers = bodyMembers

-- | Where a 'Body' is going, in world coordinates.
newtype Velocity = Velocity WVec
  deriving (Eq, Show)

instance Component Velocity where
  type Storage Velocity = B3Space Velocity

instance (MonadIO m, Has w m Physics) => Has w m Velocity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Velocity) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Velocity . B3Body.getLinearVelocity

instance (MonadIO m) => ExplSet m (B3Space Velocity) where
  explSet sp ety (Velocity v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setLinearVelocity b v

instance (MonadIO m) => ExplMembers m (B3Space Velocity) where
  explMembers = bodyMembers

{- | A 'Body'\'s orientation quaternion. Setting it normalizes the
quaternion on the way in (the engine requires unit rotations); setting
a zero quaternion is a no-op.
-}
newtype Rotation = Rotation Quat
  deriving (Eq, Show)

-- | 'Nothing' for a zero (or NaN) quaternion, which has no direction.
normalizeQuat :: Quat -> Maybe Quat
normalizeQuat (Quat (Vec3 x y z) w)
  | m2 > 0 = Just (Quat (Vec3 (x / m) (y / m) (z / m)) (w / m))
  | otherwise = Nothing
  where
    m2 = x * x + y * y + z * z + w * w
    m = sqrt m2

instance Component Rotation where
  type Storage Rotation = B3Space Rotation

instance (MonadIO m, Has w m Physics) => Has w m Rotation where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Rotation) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Rotation . B3Body.getRotation

instance (MonadIO m) => ExplSet m (B3Space Rotation) where
  explSet sp ety (Rotation q) = liftIO $
    overBody sp ety $ \b ->
      -- the engine asserts unit rotations; normalize so hand-built or
      -- interpolated quaternions are safe to set
      forM_ (normalizeQuat q) $ \q' -> do
        pos <- B3Body.getPosition b
        B3Body.setTransform b pos q'

instance (MonadIO m) => ExplMembers m (B3Space Rotation) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity, in radians per second about each axis.
newtype AngularVelocity = AngularVelocity Vec3
  deriving (Eq, Show)

instance Component AngularVelocity where
  type Storage AngularVelocity = B3Space AngularVelocity

instance (MonadIO m, Has w m Physics) => Has w m AngularVelocity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space AngularVelocity) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap AngularVelocity . B3Body.getAngularVelocity

instance (MonadIO m) => ExplSet m (B3Space AngularVelocity) where
  explSet sp ety (AngularVelocity omega) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAngularVelocity b omega

instance (MonadIO m) => ExplMembers m (B3Space AngularVelocity) where
  explMembers = bodyMembers

{- | The mass of a 'Body'. Read-only: Box3D computes it from the attached
shapes' densities.
-}
newtype BodyMass = BodyMass Float
  deriving (Eq, Show)

instance Component BodyMass where
  type Storage BodyMass = B3Space BodyMass

instance (MonadIO m, Has w m Physics) => Has w m BodyMass where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BodyMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyMass . B3Body.getMass

instance (MonadIO m) => ExplMembers m (B3Space BodyMass) where
  explMembers = bodyMembers

{- | Write-only: setting it applies a force to the 'Body'\'s center.
Forces are additive and reset by the next 'Apecs.Box3D.Space.stepPhysics'.
-}
newtype Force = Force WVec
  deriving (Eq, Show)

instance Component Force where
  type Storage Force = B3Space Force

instance (MonadIO m, Has w m Physics) => Has w m Force where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Force) where
  explSet sp ety (Force v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyForceToCenter b v True

{- | Write-only: setting it applies a torque to the 'Body'. Torques are
additive and reset by the next 'Apecs.Box3D.Space.stepPhysics'.
-}
newtype Torque = Torque Vec3
  deriving (Eq, Show)

instance Component Torque where
  type Storage Torque = B3Space Torque

instance (MonadIO m, Has w m Physics) => Has w m Torque where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space Torque) where
  explSet sp ety (Torque t) = liftIO $
    overBody sp ety $
      \b -> B3Body.applyTorque b t True

-- | Write-only: setting it applies an impulse to the 'Body'\'s center.
newtype LinearImpulse = LinearImpulse WVec
  deriving (Eq, Show)

instance Component LinearImpulse where
  type Storage LinearImpulse = B3Space LinearImpulse

instance (MonadIO m, Has w m Physics) => Has w m LinearImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space LinearImpulse) where
  explSet sp ety (LinearImpulse v) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyLinearImpulseToCenter b v True

-- | Write-only: setting it applies an angular impulse to the 'Body'.
newtype AngularImpulse = AngularImpulse Vec3
  deriving (Eq, Show)

instance Component AngularImpulse where
  type Storage AngularImpulse = B3Space AngularImpulse

instance (MonadIO m, Has w m Physics) => Has w m AngularImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space AngularImpulse) where
  explSet sp ety (AngularImpulse i) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyAngularImpulse b i True

{- | Write-only: setting it applies a force to the 'Body' at a world
point; applying off the center of mass also induces spin. Forces are
additive and reset by the next 'Apecs.Box3D.Space.stepPhysics'.
-}
data ForceAt = ForceAt WVec WVec
  deriving (Eq, Show)

instance Component ForceAt where
  type Storage ForceAt = B3Space ForceAt

instance (MonadIO m, Has w m Physics) => Has w m ForceAt where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space ForceAt) where
  explSet sp ety (ForceAt v p) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyForce b v p True

{- | Write-only: setting it applies an impulse to the 'Body' at a world
point; applying off the center of mass also induces spin.
-}
data ImpulseAt = ImpulseAt WVec WVec
  deriving (Eq, Show)

instance Component ImpulseAt where
  type Storage ImpulseAt = B3Space ImpulseAt

instance (MonadIO m, Has w m Physics) => Has w m ImpulseAt where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space ImpulseAt) where
  explSet sp ety (ImpulseAt v p) = liftIO $
    overBody sp ety $ \b ->
      B3Body.applyLinearImpulse b v p True

{- | Write-only: setting it sets a kinematic 'Body'\'s velocity so it reaches
the given world position and rotation over the given time step — pass the
time delta of your next 'Apecs.Box3D.Space.stepPhysics' call. This is the engine path for
moving platforms: unlike teleporting via 'Position', the body carries real
velocity, so it pushes and carries riders. The target is skipped when the
implied velocity is below the sleep threshold; otherwise the body is woken
if asleep, but only when the movement is significant.
-}
data TargetTransform = TargetTransform WVec Quat Float
  deriving (Eq, Show)

instance Component TargetTransform where
  type Storage TargetTransform = B3Space TargetTransform

instance (MonadIO m, Has w m Physics) => Has w m TargetTransform where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplSet m (B3Space TargetTransform) where
  explSet sp ety (TargetTransform p q dt) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setTargetTransform b (Transform p q) dt True

-- | A 'Body'\'s linear velocity damping.
newtype LinearDamping = LinearDamping Float
  deriving (Eq, Show)

instance Component LinearDamping where
  type Storage LinearDamping = B3Space LinearDamping

instance (MonadIO m, Has w m Physics) => Has w m LinearDamping where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space LinearDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap LinearDamping . B3Body.getLinearDamping

instance (MonadIO m) => ExplSet m (B3Space LinearDamping) where
  explSet sp ety (LinearDamping d) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setLinearDamping b d

instance (MonadIO m) => ExplMembers m (B3Space LinearDamping) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity damping.
newtype AngularDamping = AngularDamping Float
  deriving (Eq, Show)

instance Component AngularDamping where
  type Storage AngularDamping = B3Space AngularDamping

instance (MonadIO m, Has w m Physics) => Has w m AngularDamping where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space AngularDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap AngularDamping . B3Body.getAngularDamping

instance (MonadIO m) => ExplSet m (B3Space AngularDamping) where
  explSet sp ety (AngularDamping d) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAngularDamping b d

instance (MonadIO m) => ExplMembers m (B3Space AngularDamping) where
  explMembers = bodyMembers

-- | How strongly gravity affects a 'Body'; 1 is normal, 0 disables it.
newtype GravityScale = GravityScale Float
  deriving (Eq, Show)

instance Component GravityScale where
  type Storage GravityScale = B3Space GravityScale

instance (MonadIO m, Has w m Physics) => Has w m GravityScale where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space GravityScale) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap GravityScale . B3Body.getGravityScale

instance (MonadIO m) => ExplSet m (B3Space GravityScale) where
  explSet sp ety (GravityScale g) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setGravityScale b g

instance (MonadIO m) => ExplMembers m (B3Space GravityScale) where
  explMembers = bodyMembers

{- | Continuous collision detection for this body (the engine's "bullet"
flag): keeps small, fast bodies from tunnelling through other dynamic
bodies between substeps. Off by default; the cost scales with speed.
-}
newtype BulletBody = BulletBody Bool
  deriving (Eq, Show)

instance Component BulletBody where
  type Storage BulletBody = B3Space BulletBody

instance (MonadIO m, Has w m Physics) => Has w m BulletBody where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BulletBody) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BulletBody . B3Body.isBullet

instance (MonadIO m) => ExplSet m (B3Space BulletBody) where
  explSet sp ety (BulletBody b) = liftIO $
    overBody sp ety $ \bd ->
      B3Body.setBullet bd b

instance (MonadIO m) => ExplMembers m (B3Space BulletBody) where
  explMembers = bodyMembers

{- | Whether a 'Body' participates in the simulation at all (on by
default). Disabling removes the body and its shapes from the world
without destroying them — cheap despawn/pooling; enabling puts them
back.
-}
newtype BodyEnabled = BodyEnabled Bool
  deriving (Eq, Show)

instance Component BodyEnabled where
  type Storage BodyEnabled = B3Space BodyEnabled

instance (MonadIO m, Has w m Physics) => Has w m BodyEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BodyEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyEnabled . B3Body.isEnabled

instance (MonadIO m) => ExplSet m (B3Space BodyEnabled) where
  explSet sp ety (BodyEnabled e) = liftIO $
    overBody sp ety $ \b ->
      if e then B3Body.enable b else B3Body.disable b

instance (MonadIO m) => ExplMembers m (B3Space BodyEnabled) where
  explMembers = bodyMembers

{- | Whether a 'Body' is currently awake and simulating. Set it to wake
a body explicitly — e.g. after teleporting it via 'Position' — or to
put it to sleep. Waking or sleeping a body extends to the whole island
of bodies touching it.
-}
newtype Awake = Awake Bool
  deriving (Eq, Show)

instance Component Awake where
  type Storage Awake = B3Space Awake

instance (MonadIO m, Has w m Physics) => Has w m Awake where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Awake) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Awake . B3Body.isAwake

instance (MonadIO m) => ExplSet m (B3Space Awake) where
  explSet sp ety (Awake a) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setAwake b a

instance (MonadIO m) => ExplMembers m (B3Space Awake) where
  explMembers = bodyMembers

{- | Per-axis motion locks on a 'Body': locking a linear axis prevents
translation along it, and locking an angular axis prevents rotation
about it. Locking all three angular axes is the 3D analog of Box2D's
"fixed rotation" (contacts and off-center forces can't spin the body);
locking a single linear axis constrains movement to a plane. All axes
are unlocked by default.
-}
data MotionLocks = MotionLocks
  { linearX :: Bool
  , linearY :: Bool
  , linearZ :: Bool
  , angularX :: Bool
  , angularY :: Bool
  , angularZ :: Bool
  }
  deriving (Eq, Show)

toB3MotionLocks :: MotionLocks -> B3Body.MotionLocks
toB3MotionLocks (MotionLocks lx ly lz ax ay az) =
  B3Body.MotionLocks (fromBool lx) (fromBool ly) (fromBool lz) (fromBool ax) (fromBool ay) (fromBool az)

fromB3MotionLocks :: B3Body.MotionLocks -> MotionLocks
fromB3MotionLocks (B3Body.MotionLocks lx ly lz ax ay az) =
  MotionLocks (toBool lx) (toBool ly) (toBool lz) (toBool ax) (toBool ay) (toBool az)

instance Component MotionLocks where
  type Storage MotionLocks = B3Space MotionLocks

instance (MonadIO m, Has w m Physics) => Has w m MotionLocks where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MotionLocks) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap fromB3MotionLocks . B3Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B3Space MotionLocks) where
  explSet sp ety locks = liftIO $
    overBody sp ety $ \b ->
      B3Body.setMotionLocks b (toB3MotionLocks locks)

instance (MonadIO m) => ExplMembers m (B3Space MotionLocks) where
  explMembers = bodyMembers

{- | Whether a 'Body' may fall asleep at all (on by default). Disabling
it wakes the body (and its island). World-level control is
'Apecs.Box3D.Space.SleepingEnabled'.
-}
newtype SleepEnabled = SleepEnabled Bool
  deriving (Eq, Show)

instance Component SleepEnabled where
  type Storage SleepEnabled = B3Space SleepEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepEnabled . B3Body.isSleepEnabled

instance (MonadIO m) => ExplSet m (B3Space SleepEnabled) where
  explSet sp ety (SleepEnabled e) = liftIO $
    overBody sp ety $ \b ->
      B3Body.enableSleep b e

instance (MonadIO m) => ExplMembers m (B3Space SleepEnabled) where
  explMembers = bodyMembers

{- | The speed below which a 'Body' may fall asleep, usually in meters
per second.
-}
newtype SleepThreshold = SleepThreshold Float
  deriving (Eq, Show)

instance Component SleepThreshold where
  type Storage SleepThreshold = B3Space SleepThreshold

instance (MonadIO m, Has w m Physics) => Has w m SleepThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepThreshold) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepThreshold . B3Body.getSleepThreshold

instance (MonadIO m) => ExplSet m (B3Space SleepThreshold) where
  explSet sp ety (SleepThreshold t) = liftIO $
    overBody sp ety $ \b ->
      B3Body.setSleepThreshold b t

instance (MonadIO m) => ExplMembers m (B3Space SleepThreshold) where
  explMembers = bodyMembers

{- | The center of mass of a 'Body' in local (body) space. Read-only:
Box3D computes it from the attached shapes' densities. The
apecs-physics analog is @CenterOfGravity@.
-}
newtype CenterOfMass = CenterOfMass BVec
  deriving (Eq, Show)

instance Component CenterOfMass where
  type Storage CenterOfMass = B3Space CenterOfMass

instance (MonadIO m, Has w m Physics) => Has w m CenterOfMass where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CenterOfMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap CenterOfMass . B3Body.getLocalCenter

instance (MonadIO m) => ExplMembers m (B3Space CenterOfMass) where
  explMembers = bodyMembers

{- | The rotational inertia tensor of a 'Body' about its center of mass,
in local (body) space, usually in kg*m^2. 'Matrix3' stores it as three
columns @cx@, @cy@, @cz@. Read-only: Box3D computes it from the attached
shapes' densities. The apecs-physics analog is @Moment@, which is a
scalar because apecs-physics is 2D.
-}
newtype RotationalInertia = RotationalInertia Matrix3
  deriving (Eq, Show)

instance Component RotationalInertia where
  type Storage RotationalInertia = B3Space RotationalInertia

instance (MonadIO m, Has w m Physics) => Has w m RotationalInertia where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space RotationalInertia) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap RotationalInertia . B3Body.getLocalRotationalInertia

instance (MonadIO m) => ExplMembers m (B3Space RotationalInertia) where
  explMembers = bodyMembers

{- | An optional name for a 'Body', for debugging\/tooling. The engine
stores names in a fixed 18-byte buffer (@B3_BODY_NAME_LENGTH@); longer
names are silently truncated to 18 bytes on write, excluding the
terminating null.
-}
newtype BodyName = BodyName String
  deriving (Eq, Show)

instance Component BodyName where
  type Storage BodyName = B3Space BodyName

instance (MonadIO m, Has w m Physics) => Has w m BodyName where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space BodyName) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ \b ->
    BodyName <$> (B3Body.getName b >>= peekCString)

instance (MonadIO m) => ExplSet m (B3Space BodyName) where
  explSet sp ety (BodyName name) = liftIO $
    overBody sp ety $ \b ->
      withCString name (B3Body.setName b)

instance (MonadIO m) => ExplMembers m (B3Space BodyName) where
  explMembers = bodyMembers
