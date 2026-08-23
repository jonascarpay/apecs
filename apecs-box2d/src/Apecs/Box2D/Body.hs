{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| The 'Body' component and its sub-components, reading and writing
the engine body directly.
-}
module Apecs.Box2D.Body where

import Apecs
import Apecs.Core
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Utils (fromBool, toBool)

import Box2D.Body qualified as B2Body
import Box2D.Id (BodyId)
import Box2D.MathFunctions (makeRot, rotGetAngle)
import Box2D.MathTypes (Transform (..))
import Box2D.UserData (setUserIndex)

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

{- | Gives an entity a Box2D body. Deleting it also deletes the shapes
attached to it. A body carries the sub-components 'Position',
'Velocity', 'Angle', 'AngularVelocity', 'BodyMass', 'Force' and
'Torque'; they exist as long as the entity has a @Body@, and setting
them on an entity without one does nothing.
-}
data Body = DynamicBody | KinematicBody | StaticBody
  deriving (Eq, Ord, Enum, Show)

toB2BodyType :: Body -> B2Body.BodyType
toB2BodyType DynamicBody = B2Body.DynamicBody
toB2BodyType KinematicBody = B2Body.KinematicBody
toB2BodyType StaticBody = B2Body.StaticBody

fromB2BodyType :: B2Body.BodyType -> Body
fromB2BodyType ty = case ty of
  B2Body.DynamicBody -> DynamicBody
  B2Body.KinematicBody -> KinematicBody
  B2Body.StaticBody -> StaticBody

instance Component Body where
  type Storage Body = B2Space Body

instance (MonadIO m, Has w m Physics) => Has w m Body where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Body) where
  explSet sp ety btype = liftIO $ do
    bodies <- readIORef sp.bodies
    case IM.lookup ety bodies of
      Just b -> B2Body.setType b (toB2BodyType btype)
      Nothing -> do
        b <- B2Body.create sp.world (sp.bodyDef){B2Body.type_ = toB2BodyType btype}
        setUserIndex b ety
        modifyIORef' sp.bodies (IM.insert ety b)

instance (MonadIO m) => ExplGet m (B2Space Body) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap fromB2BodyType . B2Body.getType

instance (MonadIO m) => ExplDestroy m (B2Space Body) where
  explDestroy sp ety = liftIO $ do
    bodies <- readIORef sp.bodies
    forM_ (IM.lookup ety bodies) $ \b -> do
      -- the engine destroys attached shapes, joints and chains along with
      -- the body; the reverse indices name their entity records to drop,
      -- so a destroy costs O(dependents), not a scan of whole registries
      shapeDeps <- IM.findWithDefault IS.empty ety <$> readIORef sp.shapesByBody
      chainDeps <- IM.findWithDefault IS.empty ety <$> readIORef sp.chainsByBody
      jointDeps <- IM.findWithDefault IS.empty ety <$> readIORef sp.jointsByBody
      modifyIORef' sp.shapes (`IM.withoutKeys` shapeDeps)
      modifyIORef' sp.chains (`IM.withoutKeys` chainDeps)
      modifyIORef' sp.shapesByBody (IM.delete ety)
      modifyIORef' sp.chainsByBody (IM.delete ety)
      -- a joint hangs off two bodies: trim it from the counterpart
      -- body's index (this body's whole entry is dropped below)
      joints <- readIORef sp.joints
      forM_ (IS.toList jointDeps) $ \jEty ->
        forM_ (IM.lookup jEty joints) $ \(JointRecord _ (Joint (Entity a) (Entity b') _) _) ->
          depDelete sp.jointsByBody (if a == ety then b' else a) jEty
      modifyIORef' sp.jointsByBody (IM.delete ety)
      modifyIORef' sp.joints (`IM.withoutKeys` jointDeps)
      modifyIORef' sp.bodies (IM.delete ety)
      B2Body.destroy b

instance (MonadIO m) => ExplMembers m (B2Space Body) where
  explMembers = bodyMembers

-- | The raw Box2D body of an entity, for use with "Box2D.Body" directly.
newtype B2BodyId = B2BodyId BodyId
  deriving (Eq, Show)

instance Component B2BodyId where
  type Storage B2BodyId = B2Space B2BodyId

instance (MonadIO m, Has w m Physics) => Has w m B2BodyId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2BodyId) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety (pure . B2BodyId)

instance (MonadIO m) => ExplMembers m (B2Space B2BodyId) where
  explMembers = bodyMembers

-- Body sub-components ------------------------------------------------------

-- | Where a 'Body' is, in world coordinates.
newtype Position = Position WVec
  deriving (Eq, Show)

instance Component Position where
  type Storage Position = B2Space Position

instance (MonadIO m, Has w m Physics) => Has w m Position where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Position) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Position . B2Body.getPosition

instance (MonadIO m) => ExplSet m (B2Space Position) where
  explSet sp ety (Position p) = liftIO $
    overBody sp ety $ \b -> do
      rot <- B2Body.getRotation b
      B2Body.setTransform b p rot

instance (MonadIO m) => ExplMembers m (B2Space Position) where
  explMembers = bodyMembers

-- | Where a 'Body' is going, in world coordinates.
newtype Velocity = Velocity WVec
  deriving (Eq, Show)

instance Component Velocity where
  type Storage Velocity = B2Space Velocity

instance (MonadIO m, Has w m Physics) => Has w m Velocity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Velocity) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Velocity . B2Body.getLinearVelocity

instance (MonadIO m) => ExplSet m (B2Space Velocity) where
  explSet sp ety (Velocity v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setLinearVelocity b v

instance (MonadIO m) => ExplMembers m (B2Space Velocity) where
  explMembers = bodyMembers

-- | A 'Body'\'s rotation, in radians.
newtype Angle = Angle Float
  deriving (Eq, Show)

instance Component Angle where
  type Storage Angle = B2Space Angle

instance (MonadIO m, Has w m Physics) => Has w m Angle where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Angle) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap (Angle . rotGetAngle) . B2Body.getRotation

instance (MonadIO m) => ExplSet m (B2Space Angle) where
  explSet sp ety (Angle theta) = liftIO $
    overBody sp ety $ \b -> do
      pos <- B2Body.getPosition b
      rot <- makeRot theta
      B2Body.setTransform b pos rot

instance (MonadIO m) => ExplMembers m (B2Space Angle) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity, in radians per second.
newtype AngularVelocity = AngularVelocity Float
  deriving (Eq, Show)

instance Component AngularVelocity where
  type Storage AngularVelocity = B2Space AngularVelocity

instance (MonadIO m, Has w m Physics) => Has w m AngularVelocity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space AngularVelocity) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap AngularVelocity . B2Body.getAngularVelocity

instance (MonadIO m) => ExplSet m (B2Space AngularVelocity) where
  explSet sp ety (AngularVelocity omega) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAngularVelocity b omega

instance (MonadIO m) => ExplMembers m (B2Space AngularVelocity) where
  explMembers = bodyMembers

{- | The mass of a 'Body'. Read-only: Box2D computes it from the attached
shapes' densities.
-}
newtype BodyMass = BodyMass Float
  deriving (Eq, Show)

instance Component BodyMass where
  type Storage BodyMass = B2Space BodyMass

instance (MonadIO m, Has w m Physics) => Has w m BodyMass where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyMass . B2Body.getMass

instance (MonadIO m) => ExplMembers m (B2Space BodyMass) where
  explMembers = bodyMembers

{- | Write-only: setting it applies a force to the 'Body'\'s center.
Forces are additive and reset by the next 'Apecs.Box2D.Space.stepPhysics'.
-}
newtype Force = Force WVec
  deriving (Eq, Show)

instance Component Force where
  type Storage Force = B2Space Force

instance (MonadIO m, Has w m Physics) => Has w m Force where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Force) where
  explSet sp ety (Force v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyForceToCenter b v True

{- | Write-only: setting it applies a torque to the 'Body'. Torques are
additive and reset by the next 'Apecs.Box2D.Space.stepPhysics'.
-}
newtype Torque = Torque Float
  deriving (Eq, Show)

instance Component Torque where
  type Storage Torque = B2Space Torque

instance (MonadIO m, Has w m Physics) => Has w m Torque where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space Torque) where
  explSet sp ety (Torque t) = liftIO $
    overBody sp ety $
      \b -> B2Body.applyTorque b t True

-- | Write-only: setting it applies an impulse to the 'Body'\'s center.
newtype LinearImpulse = LinearImpulse WVec
  deriving (Eq, Show)

instance Component LinearImpulse where
  type Storage LinearImpulse = B2Space LinearImpulse

instance (MonadIO m, Has w m Physics) => Has w m LinearImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space LinearImpulse) where
  explSet sp ety (LinearImpulse v) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyLinearImpulseToCenter b v True

-- | Write-only: setting it applies an angular impulse to the 'Body'.
newtype AngularImpulse = AngularImpulse Float
  deriving (Eq, Show)

instance Component AngularImpulse where
  type Storage AngularImpulse = B2Space AngularImpulse

instance (MonadIO m, Has w m Physics) => Has w m AngularImpulse where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space AngularImpulse) where
  explSet sp ety (AngularImpulse i) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyAngularImpulse b i True

{- | Write-only: setting it applies a force to the 'Body' at a world
point; applying off the center of mass also induces spin. Forces are
additive and reset by the next 'Apecs.Box2D.Space.stepPhysics'.
-}
data ForceAt = ForceAt WVec WVec
  deriving (Eq, Show)

instance Component ForceAt where
  type Storage ForceAt = B2Space ForceAt

instance (MonadIO m, Has w m Physics) => Has w m ForceAt where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space ForceAt) where
  explSet sp ety (ForceAt v p) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyForce b v p True

{- | Write-only: setting it applies an impulse to the 'Body' at a world
point; applying off the center of mass also induces spin.
-}
data ImpulseAt = ImpulseAt WVec WVec
  deriving (Eq, Show)

instance Component ImpulseAt where
  type Storage ImpulseAt = B2Space ImpulseAt

instance (MonadIO m, Has w m Physics) => Has w m ImpulseAt where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space ImpulseAt) where
  explSet sp ety (ImpulseAt v p) = liftIO $
    overBody sp ety $ \b ->
      B2Body.applyLinearImpulse b v p True

{- | Write-only: setting it sets a kinematic 'Body'\'s velocity so it reaches
the given world position and angle (radians) over the given time step — pass
the time delta of your next 'Apecs.Box2D.Space.stepPhysics' call. This is the engine path for
moving platforms: unlike teleporting via 'Position', the body carries real
velocity, so it pushes and carries riders. The target is skipped when the
body is asleep and the implied velocity is below the sleep threshold.
-}
data TargetTransform = TargetTransform WVec Float Float
  deriving (Eq, Show)

instance Component TargetTransform where
  type Storage TargetTransform = B2Space TargetTransform

instance (MonadIO m, Has w m Physics) => Has w m TargetTransform where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplSet m (B2Space TargetTransform) where
  explSet sp ety (TargetTransform p theta dt) = liftIO $
    overBody sp ety $ \b -> do
      rot <- makeRot theta
      B2Body.setTargetTransform b (Transform p rot) dt True

-- | A 'Body'\'s linear velocity damping.
newtype LinearDamping = LinearDamping Float
  deriving (Eq, Show)

instance Component LinearDamping where
  type Storage LinearDamping = B2Space LinearDamping

instance (MonadIO m, Has w m Physics) => Has w m LinearDamping where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space LinearDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap LinearDamping . B2Body.getLinearDamping

instance (MonadIO m) => ExplSet m (B2Space LinearDamping) where
  explSet sp ety (LinearDamping d) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setLinearDamping b d

instance (MonadIO m) => ExplMembers m (B2Space LinearDamping) where
  explMembers = bodyMembers

-- | A 'Body'\'s angular velocity damping.
newtype AngularDamping = AngularDamping Float
  deriving (Eq, Show)

instance Component AngularDamping where
  type Storage AngularDamping = B2Space AngularDamping

instance (MonadIO m, Has w m Physics) => Has w m AngularDamping where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space AngularDamping) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap AngularDamping . B2Body.getAngularDamping

instance (MonadIO m) => ExplSet m (B2Space AngularDamping) where
  explSet sp ety (AngularDamping d) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAngularDamping b d

instance (MonadIO m) => ExplMembers m (B2Space AngularDamping) where
  explMembers = bodyMembers

-- | How strongly gravity affects a 'Body'; 1 is normal, 0 disables it.
newtype GravityScale = GravityScale Float
  deriving (Eq, Show)

instance Component GravityScale where
  type Storage GravityScale = B2Space GravityScale

instance (MonadIO m, Has w m Physics) => Has w m GravityScale where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space GravityScale) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap GravityScale . B2Body.getGravityScale

instance (MonadIO m) => ExplSet m (B2Space GravityScale) where
  explSet sp ety (GravityScale g) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setGravityScale b g

instance (MonadIO m) => ExplMembers m (B2Space GravityScale) where
  explMembers = bodyMembers

{- | Continuous collision detection for this body (the engine's "bullet"
flag): keeps small, fast bodies from tunnelling through other dynamic
bodies between substeps. Off by default; the cost scales with speed.
-}
newtype BulletBody = BulletBody Bool
  deriving (Eq, Show)

instance Component BulletBody where
  type Storage BulletBody = B2Space BulletBody

instance (MonadIO m, Has w m Physics) => Has w m BulletBody where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BulletBody) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BulletBody . B2Body.isBullet

instance (MonadIO m) => ExplSet m (B2Space BulletBody) where
  explSet sp ety (BulletBody b) = liftIO $
    overBody sp ety $ \bd ->
      B2Body.setBullet bd b

instance (MonadIO m) => ExplMembers m (B2Space BulletBody) where
  explMembers = bodyMembers

{- | Whether a 'Body' participates in the simulation at all (on by
default). Disabling removes the body and its shapes from the world
without destroying them — cheap despawn/pooling; enabling puts them
back.
-}
newtype BodyEnabled = BodyEnabled Bool
  deriving (Eq, Show)

instance Component BodyEnabled where
  type Storage BodyEnabled = B2Space BodyEnabled

instance (MonadIO m, Has w m Physics) => Has w m BodyEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap BodyEnabled . B2Body.isEnabled

instance (MonadIO m) => ExplSet m (B2Space BodyEnabled) where
  explSet sp ety (BodyEnabled e) = liftIO $
    overBody sp ety $ \b ->
      if e then B2Body.enable b else B2Body.disable b

instance (MonadIO m) => ExplMembers m (B2Space BodyEnabled) where
  explMembers = bodyMembers

{- | Whether a 'Body' is currently awake and simulating. Set it to wake
a body explicitly — e.g. after teleporting it via 'Position' — or to
put it to sleep. Waking or sleeping a body extends to the whole island
of bodies touching it.
-}
newtype Awake = Awake Bool
  deriving (Eq, Show)

instance Component Awake where
  type Storage Awake = B2Space Awake

instance (MonadIO m, Has w m Physics) => Has w m Awake where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Awake) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap Awake . B2Body.isAwake

instance (MonadIO m) => ExplSet m (B2Space Awake) where
  explSet sp ety (Awake a) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setAwake b a

instance (MonadIO m) => ExplMembers m (B2Space Awake) where
  explMembers = bodyMembers

{- | Per-axis motion locks on a 'Body': locking a linear axis prevents
translation along it, and locking the angular axis prevents rotation
about it. Locked rotation is the classic platformer/top-down "fixed
rotation" (see 'FixedRotation'); locking a linear axis constrains a
body to rail-style movement along the other. All axes are unlocked by
default.
-}
data MotionLocks = MotionLocks
  { linearX :: Bool
  , linearY :: Bool
  , angularZ :: Bool
  }
  deriving (Eq, Show)

toB2MotionLocks :: MotionLocks -> B2Body.MotionLocks
toB2MotionLocks (MotionLocks lx ly az) =
  B2Body.MotionLocks (fromBool lx) (fromBool ly) (fromBool az)

fromB2MotionLocks :: B2Body.MotionLocks -> MotionLocks
fromB2MotionLocks (B2Body.MotionLocks lx ly az) =
  MotionLocks (toBool lx) (toBool ly) (toBool az)

instance Component MotionLocks where
  type Storage MotionLocks = B2Space MotionLocks

instance (MonadIO m, Has w m Physics) => Has w m MotionLocks where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MotionLocks) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap fromB2MotionLocks . B2Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B2Space MotionLocks) where
  explSet sp ety locks = liftIO $
    overBody sp ety $ \b ->
      B2Body.setMotionLocks b (toB2MotionLocks locks)

instance (MonadIO m) => ExplMembers m (B2Space MotionLocks) where
  explMembers = bodyMembers

{- | Whether a 'Body'\'s rotation is locked: top-down and platformer
characters lock rotation so contacts and off-center forces can't spin
them. Sugar over the 'MotionLocks' angular-Z lock; setting it preserves
the linear locks.
-}
newtype FixedRotation = FixedRotation Bool
  deriving (Eq, Show)

instance Component FixedRotation where
  type Storage FixedRotation = B2Space FixedRotation

instance (MonadIO m, Has w m Physics) => Has w m FixedRotation where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space FixedRotation) where
  explExists = bodyExists
  explGet sp ety =
    liftIO $
      withBody sp ety $
        fmap (FixedRotation . toBool . (.angularZ)) . B2Body.getMotionLocks

instance (MonadIO m) => ExplSet m (B2Space FixedRotation) where
  explSet sp ety (FixedRotation fixed) = liftIO $
    overBody sp ety $ \b -> do
      locks <- B2Body.getMotionLocks b
      B2Body.setMotionLocks b locks{B2Body.angularZ = fromBool fixed}

instance (MonadIO m) => ExplMembers m (B2Space FixedRotation) where
  explMembers = bodyMembers

{- | Whether a 'Body' may fall asleep at all (on by default). Disabling
it wakes the body (and its island). World-level control is
'Apecs.Box2D.Space.SleepingEnabled'.
-}
newtype SleepEnabled = SleepEnabled Bool
  deriving (Eq, Show)

instance Component SleepEnabled where
  type Storage SleepEnabled = B2Space SleepEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepEnabled) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepEnabled . B2Body.isSleepEnabled

instance (MonadIO m) => ExplSet m (B2Space SleepEnabled) where
  explSet sp ety (SleepEnabled e) = liftIO $
    overBody sp ety $ \b ->
      B2Body.enableSleep b e

instance (MonadIO m) => ExplMembers m (B2Space SleepEnabled) where
  explMembers = bodyMembers

{- | The speed below which a 'Body' may fall asleep, usually in meters
per second.
-}
newtype SleepThreshold = SleepThreshold Float
  deriving (Eq, Show)

instance Component SleepThreshold where
  type Storage SleepThreshold = B2Space SleepThreshold

instance (MonadIO m, Has w m Physics) => Has w m SleepThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepThreshold) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap SleepThreshold . B2Body.getSleepThreshold

instance (MonadIO m) => ExplSet m (B2Space SleepThreshold) where
  explSet sp ety (SleepThreshold t) = liftIO $
    overBody sp ety $ \b ->
      B2Body.setSleepThreshold b t

instance (MonadIO m) => ExplMembers m (B2Space SleepThreshold) where
  explMembers = bodyMembers

{- | The center of mass of a 'Body' in local (body) space. Read-only:
Box2D computes it from the attached shapes' densities. The
apecs-physics analog is @CenterOfGravity@.
-}
newtype CenterOfMass = CenterOfMass BVec
  deriving (Eq, Show)

instance Component CenterOfMass where
  type Storage CenterOfMass = B2Space CenterOfMass

instance (MonadIO m, Has w m Physics) => Has w m CenterOfMass where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CenterOfMass) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap CenterOfMass . B2Body.getLocalCenter

instance (MonadIO m) => ExplMembers m (B2Space CenterOfMass) where
  explMembers = bodyMembers

{- | The rotational inertia of a 'Body', usually in kg*m^2. Read-only:
Box2D computes it from the attached shapes' densities. The
apecs-physics analog is @Moment@.
-}
newtype RotationalInertia = RotationalInertia Float
  deriving (Eq, Show)

instance Component RotationalInertia where
  type Storage RotationalInertia = B2Space RotationalInertia

instance (MonadIO m, Has w m Physics) => Has w m RotationalInertia where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space RotationalInertia) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ fmap RotationalInertia . B2Body.getRotationalInertia

instance (MonadIO m) => ExplMembers m (B2Space RotationalInertia) where
  explMembers = bodyMembers

{- | An optional name for a 'Body', for debugging\/tooling. The engine
stores names in a fixed 10-byte buffer (@B2_NAME_LENGTH@); longer names
are silently truncated to 10 bytes on write, excluding the terminating
null.
-}
newtype BodyName = BodyName String
  deriving (Eq, Show)

instance Component BodyName where
  type Storage BodyName = B2Space BodyName

instance (MonadIO m, Has w m Physics) => Has w m BodyName where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space BodyName) where
  explExists = bodyExists
  explGet sp ety = liftIO $ withBody sp ety $ \b ->
    BodyName <$> (B2Body.getName b >>= peekCString)

instance (MonadIO m) => ExplSet m (B2Space BodyName) where
  explSet sp ety (BodyName name) = liftIO $
    overBody sp ety $ \b ->
      withCString name (B2Body.setName b)

instance (MonadIO m) => ExplMembers m (B2Space BodyName) where
  explMembers = bodyMembers
