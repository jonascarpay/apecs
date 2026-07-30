{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| World-level systems and components: stepping and teardown,
explosions, and the world tuning knobs ('Gravity', 'Substeps', ...).
-}
module Apecs.Box3D.Space where

import Apecs
import Apecs.Core
import Control.Monad.IO.Class (MonadIO)
import Data.IORef

import Box3D.Id (WorldId)
import Box3D.MathTypes (Vec3 (..))
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

-- | The raw Box3D world, for use with the "Box3D" modules directly.
getWorldId :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m WorldId
getWorldId = (.world) <$> (getStore :: SystemT w m (B3Space Physics))

{- | Advance the simulation by a time delta, resolving contacts with the
'Substeps' number of substeps.
-}
stepPhysics :: forall w m. (MonadIO m, Has w m Physics) => Float -> SystemT w m ()
stepPhysics dT = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    substeps <- readIORef sp.substeps
    B3World.step sp.world dT substeps

{- | Destroy the engine world along with all its bodies and shapes, and
clear the registries. The store is unusable afterwards; call this on
teardown. Box3D keeps worlds in a fixed-size global registry, so
sessions that repeatedly create worlds (test suites, GHCi reloads) must
destroy them too or world creation eventually fails.
-}
destroyPhysics :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
destroyPhysics = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    B3World.destroy sp.world
    writeIORef sp.bodies mempty
    writeIORef sp.shapes mempty
    writeIORef sp.joints mempty
    writeIORef sp.shapesByBody mempty
    writeIORef sp.jointsByBody mempty

{- | Apply a radial impulse to every dynamic body within a radius of a
world point, as if from an explosion: each affected shape is pushed
away from the center along the line to its nearest surface point,
scaled by how much of its area faces the blast. Only spheres, capsules
and hulls receive an impulse; a body is woken even if it was asleep.
The impulse has no soft falloff by default, so it cuts off sharply at
the radius, and every shape passes the default filter (nothing is
masked out). A negative impulse pulls bodies inward instead of pushing
them.
-}
explode
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -- ^ Explosion center, in world coordinates.
  -> Float
  -- ^ Radius: shapes within this distance get the full impulse.
  -> Float
  {- ^ Impulse per unit area of shape surface facing the blast;
  negative for an implosion.
  -}
  -> SystemT w m ()
explode center radius impulse = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    def <- B3World.defaultExplosionDef
    B3World.explode
      sp.world
      def
        { B3World.position = center
        , B3World.radius = radius
        , B3World.impulsePerArea = impulse
        }

-- Space sub-components ----------------------------------------------------

-- | The world's gravity vector.
newtype Gravity = Gravity WVec
  deriving (Eq, Show)

earthGravity :: Gravity
earthGravity = Gravity (Vec3 0 (-9.81) 0)

instance Component Gravity where
  type Storage Gravity = B3Space Gravity

instance (MonadIO m, Has w m Physics) => Has w m Gravity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Gravity) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Gravity <$> B3World.getGravity sp.world

instance (MonadIO m) => ExplSet m (B3Space Gravity) where
  explSet sp _ (Gravity v) = liftIO $ B3World.setGravity sp.world v

{- | The number of contact substeps per 'stepPhysics' call. Defaults to 4;
clamped to at least 1.
-}
newtype Substeps = Substeps Int
  deriving (Eq, Show)

instance Component Substeps where
  type Storage Substeps = B3Space Substeps

instance (MonadIO m, Has w m Physics) => Has w m Substeps where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Substeps) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Substeps <$> readIORef sp.substeps

instance (MonadIO m) => ExplSet m (B3Space Substeps) where
  explSet sp _ (Substeps n) = liftIO $ writeIORef sp.substeps (max 1 n)

{- | Whether bodies in this world may fall asleep at all (on by
default). Disabling it wakes everything and saves the bookkeeping when
nothing would sleep anyway; sleeping gains performance on large scenes
where most bodies are at rest. Per-body control is 'Apecs.Box3D.Body.SleepEnabled'.
-}
newtype SleepingEnabled = SleepingEnabled Bool
  deriving (Eq, Show)

instance Component SleepingEnabled where
  type Storage SleepingEnabled = B3Space SleepingEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepingEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space SleepingEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ SleepingEnabled <$> B3World.isSleepingEnabled sp.world

instance (MonadIO m) => ExplSet m (B3Space SleepingEnabled) where
  explSet sp _ (SleepingEnabled e) = liftIO $ B3World.enableSleeping sp.world e

{- | Whether continuous collision detection runs between fast dynamic
bodies and static geometry, keeping them from tunnelling through walls
between substeps (on by default; disabling it is a minor performance
gain). Continuous detection between two dynamic bodies is a separate,
per-body opt-in: see 'Apecs.Box3D.Body.BulletBody'.
-}
newtype ContinuousEnabled = ContinuousEnabled Bool
  deriving (Eq, Show)

instance Component ContinuousEnabled where
  type Storage ContinuousEnabled = B3Space ContinuousEnabled

instance (MonadIO m, Has w m Physics) => Has w m ContinuousEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space ContinuousEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ ContinuousEnabled <$> B3World.isContinuousEnabled sp.world

instance (MonadIO m) => ExplSet m (B3Space ContinuousEnabled) where
  explSet sp _ (ContinuousEnabled e) = liftIO $ B3World.enableContinuous sp.world e

{- | The approach speed above which a contact generates a hit event,
usually in meters per second (engine default 1). Read by 'Apecs.Box3D.Collision.Impacts',
which also needs hit events enabled per shape — on by default for every
shape this layer creates.
-}
newtype HitEventThreshold = HitEventThreshold Float
  deriving (Eq, Show)

instance Component HitEventThreshold where
  type Storage HitEventThreshold = B3Space HitEventThreshold

instance (MonadIO m, Has w m Physics) => Has w m HitEventThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space HitEventThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ HitEventThreshold <$> B3World.getHitEventThreshold sp.world

instance (MonadIO m) => ExplSet m (B3Space HitEventThreshold) where
  explSet sp _ (HitEventThreshold t) = liftIO $ B3World.setHitEventThreshold sp.world t

{- | The relative approach speed below which a contact's 'Apecs.Box3D.Shape.Elasticity'
is ignored and it doesn't bounce, usually in meters per second. Don't
set this very low: contacts hovering just above the threshold keep
bouncing instead of settling, which prevents bodies from falling
asleep.
-}
newtype RestitutionThreshold = RestitutionThreshold Float
  deriving (Eq, Show)

instance Component RestitutionThreshold where
  type Storage RestitutionThreshold = B3Space RestitutionThreshold

instance (MonadIO m, Has w m Physics) => Has w m RestitutionThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space RestitutionThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ RestitutionThreshold <$> B3World.getRestitutionThreshold sp.world

instance (MonadIO m) => ExplSet m (B3Space RestitutionThreshold) where
  explSet sp _ (RestitutionThreshold t) = liftIO $ B3World.setRestitutionThreshold sp.world t

{- | The speed cap applied to every 'Apecs.Box3D.Body.Body' in this world, usually in
meters per second: velocities that would exceed it are clamped each
step. Guards against tunnelling and blow-ups from stray forces or
impulses.
-}
newtype MaximumLinearSpeed = MaximumLinearSpeed Float
  deriving (Eq, Show)

instance Component MaximumLinearSpeed where
  type Storage MaximumLinearSpeed = B3Space MaximumLinearSpeed

instance (MonadIO m, Has w m Physics) => Has w m MaximumLinearSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space MaximumLinearSpeed) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ MaximumLinearSpeed <$> B3World.getMaximumLinearSpeed sp.world

instance (MonadIO m) => ExplSet m (B3Space MaximumLinearSpeed) where
  explSet sp _ (MaximumLinearSpeed s) = liftIO $ B3World.setMaximumLinearSpeed sp.world s

{- | The number of solver worker threads the world uses (default 1).
Raising it parallelises the solver across islands; it only pays off on
scenes with many independent islands, and the program must be built
with the threaded runtime. Settable at any time between 'stepPhysics'
calls. Must be in the range [1, B3_MAX_WORKERS].
-}
newtype WorkerCount = WorkerCount Int
  deriving (Eq, Show)

instance Component WorkerCount where
  type Storage WorkerCount = B3Space WorkerCount

instance (MonadIO m, Has w m Physics) => Has w m WorkerCount where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space WorkerCount) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ WorkerCount <$> B3World.getWorkerCount sp.world

instance (MonadIO m) => ExplSet m (B3Space WorkerCount) where
  explSet sp _ (WorkerCount c) = liftIO $ B3World.setWorkerCount sp.world c
