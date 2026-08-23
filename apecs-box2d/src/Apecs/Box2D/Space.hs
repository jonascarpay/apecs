{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-| World-level systems and components: stepping and teardown,
explosions, and the world tuning knobs ('Gravity', 'Substeps', ...).
-}
module Apecs.Box2D.Space where

import Apecs
import Apecs.Core
import Control.Monad.IO.Class (MonadIO)
import Data.IORef

import Box2D.Id (WorldId)
import Box2D.MathTypes (Vec2 (..))
import Box2D.World qualified as B2World

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

-- | The raw Box2D world, for use with the "Box2D" modules directly.
getWorldId :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m WorldId
getWorldId = (.world) <$> (getStore :: SystemT w m (B2Space Physics))

{- | Advance the simulation by a time delta, resolving contacts with the
'Substeps' number of substeps.
-}
stepPhysics :: forall w m. (MonadIO m, Has w m Physics) => Float -> SystemT w m ()
stepPhysics dT = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    substeps <- readIORef sp.substeps
    B2World.step sp.world dT substeps

{- | Destroy the engine world along with all its bodies and shapes, and
clear the registries. The store is unusable afterwards; call this on
teardown. Box2D keeps worlds in a fixed-size global registry, so
sessions that repeatedly create worlds (test suites, GHCi reloads) must
destroy them too or world creation eventually fails.
-}
destroyPhysics :: forall w m. (MonadIO m, Has w m Physics) => SystemT w m ()
destroyPhysics = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    B2World.destroy sp.world
    writeIORef sp.bodies mempty
    writeIORef sp.shapes mempty
    writeIORef sp.joints mempty
    writeIORef sp.chains mempty
    writeIORef sp.shapesByBody mempty
    writeIORef sp.jointsByBody mempty
    writeIORef sp.chainsByBody mempty

{- | Apply a radial impulse to every dynamic body within a radius of a
world point, as if from an explosion: each affected shape is pushed
away from the center along the line to its nearest surface point,
scaled by how much of its perimeter faces the blast. Only circles,
capsules and polygons receive an impulse (segments do not); a body is
woken even if it was asleep. The impulse has no soft falloff by
default, so it cuts off sharply at the radius, and every shape passes
the default filter (nothing is masked out). A negative impulse pulls
bodies inward instead of pushing them.
-}
explode
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -- ^ Explosion center, in world coordinates.
  -> Float
  -- ^ Radius: shapes within this distance get the full impulse.
  -> Float
  {- ^ Impulse per unit length of shape perimeter facing the blast;
  negative for an implosion.
  -}
  -> SystemT w m ()
explode center radius impulse = do
  sp :: B2Space Physics <- getStore
  liftIO $ do
    def <- B2World.defaultExplosionDef
    B2World.explode
      sp.world
      def
        { B2World.position = center
        , B2World.radius = radius
        , B2World.impulsePerLength = impulse
        }

-- Space sub-components ----------------------------------------------------

-- | The world's gravity vector.
newtype Gravity = Gravity WVec
  deriving (Eq, Show)

earthGravity :: Gravity
earthGravity = Gravity (Vec2 0 (-9.81))

instance Component Gravity where
  type Storage Gravity = B2Space Gravity

instance (MonadIO m, Has w m Physics) => Has w m Gravity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Gravity) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Gravity <$> B2World.getGravity sp.world

instance (MonadIO m) => ExplSet m (B2Space Gravity) where
  explSet sp _ (Gravity v) = liftIO $ B2World.setGravity sp.world v

{- | The number of contact substeps per 'stepPhysics' call (the analog of
apecs-physics @Iterations@). Defaults to 4; clamped to at least 1.
-}
newtype Substeps = Substeps Int
  deriving (Eq, Show)

instance Component Substeps where
  type Storage Substeps = B2Space Substeps

instance (MonadIO m, Has w m Physics) => Has w m Substeps where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Substeps) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ Substeps <$> readIORef sp.substeps

instance (MonadIO m) => ExplSet m (B2Space Substeps) where
  explSet sp _ (Substeps n) = liftIO $ writeIORef sp.substeps (max 1 n)

{- | Whether bodies in this world may fall asleep at all (on by
default). Disabling it wakes everything and saves the bookkeeping when
nothing would sleep anyway; sleeping gains performance on large scenes
where most bodies are at rest. Per-body control is 'Apecs.Box2D.Body.SleepEnabled'.
-}
newtype SleepingEnabled = SleepingEnabled Bool
  deriving (Eq, Show)

instance Component SleepingEnabled where
  type Storage SleepingEnabled = B2Space SleepingEnabled

instance (MonadIO m, Has w m Physics) => Has w m SleepingEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space SleepingEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ SleepingEnabled <$> B2World.isSleepingEnabled sp.world

instance (MonadIO m) => ExplSet m (B2Space SleepingEnabled) where
  explSet sp _ (SleepingEnabled e) = liftIO $ B2World.enableSleeping sp.world e

{- | Whether continuous collision detection runs between fast dynamic
bodies and static geometry, keeping them from tunnelling through walls
between substeps (on by default; disabling it is a minor performance
gain). Continuous detection between two dynamic bodies is a separate,
per-body opt-in: see 'Apecs.Box2D.Body.BulletBody'.
-}
newtype ContinuousEnabled = ContinuousEnabled Bool
  deriving (Eq, Show)

instance Component ContinuousEnabled where
  type Storage ContinuousEnabled = B2Space ContinuousEnabled

instance (MonadIO m, Has w m Physics) => Has w m ContinuousEnabled where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space ContinuousEnabled) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ ContinuousEnabled <$> B2World.isContinuousEnabled sp.world

instance (MonadIO m) => ExplSet m (B2Space ContinuousEnabled) where
  explSet sp _ (ContinuousEnabled e) = liftIO $ B2World.enableContinuous sp.world e

{- | The approach speed above which a contact generates a hit event,
usually in meters per second (engine default 1). Read by 'Apecs.Box2D.Collision.Impacts',
which also needs hit events enabled per shape — on by default for every
shape this layer creates.
-}
newtype HitEventThreshold = HitEventThreshold Float
  deriving (Eq, Show)

instance Component HitEventThreshold where
  type Storage HitEventThreshold = B2Space HitEventThreshold

instance (MonadIO m, Has w m Physics) => Has w m HitEventThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space HitEventThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ HitEventThreshold <$> B2World.getHitEventThreshold sp.world

instance (MonadIO m) => ExplSet m (B2Space HitEventThreshold) where
  explSet sp _ (HitEventThreshold t) = liftIO $ B2World.setHitEventThreshold sp.world t

{- | The relative approach speed below which a contact's 'Apecs.Box2D.Shape.Elasticity'
is ignored and it doesn't bounce, usually in meters per second. Don't
set this very low: contacts hovering just above the threshold keep
bouncing instead of settling, which prevents bodies from falling
asleep.
-}
newtype RestitutionThreshold = RestitutionThreshold Float
  deriving (Eq, Show)

instance Component RestitutionThreshold where
  type Storage RestitutionThreshold = B2Space RestitutionThreshold

instance (MonadIO m, Has w m Physics) => Has w m RestitutionThreshold where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space RestitutionThreshold) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ RestitutionThreshold <$> B2World.getRestitutionThreshold sp.world

instance (MonadIO m) => ExplSet m (B2Space RestitutionThreshold) where
  explSet sp _ (RestitutionThreshold t) = liftIO $ B2World.setRestitutionThreshold sp.world t

{- | The speed cap applied to every 'Apecs.Box2D.Body.Body' in this world, usually in
meters per second: velocities that would exceed it are clamped each
step. Guards against tunnelling and blow-ups from stray forces or
impulses.
-}
newtype MaximumLinearSpeed = MaximumLinearSpeed Float
  deriving (Eq, Show)

instance Component MaximumLinearSpeed where
  type Storage MaximumLinearSpeed = B2Space MaximumLinearSpeed

instance (MonadIO m, Has w m Physics) => Has w m MaximumLinearSpeed where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space MaximumLinearSpeed) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ MaximumLinearSpeed <$> B2World.getMaximumLinearSpeed sp.world

instance (MonadIO m) => ExplSet m (B2Space MaximumLinearSpeed) where
  explSet sp _ (MaximumLinearSpeed s) = liftIO $ B2World.setMaximumLinearSpeed sp.world s

{- | The number of solver worker threads the world uses (default 1).
Raising it parallelises the solver across islands; it only pays off on
scenes with many independent islands, and the program must be built
with the threaded runtime. Settable at any time between 'stepPhysics'
calls. Must be in the range [1, B2_MAX_WORKERS].
-}
newtype WorkerCount = WorkerCount Int
  deriving (Eq, Show)

instance Component WorkerCount where
  type Storage WorkerCount = B2Space WorkerCount

instance (MonadIO m, Has w m Physics) => Has w m WorkerCount where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space WorkerCount) where
  explExists _ _ = pure True
  explGet sp _ = liftIO $ WorkerCount <$> B2World.getWorkerCount sp.world

instance (MonadIO m) => ExplSet m (B2Space WorkerCount) where
  explSet sp _ (WorkerCount c) = liftIO $ B2World.setWorkerCount sp.world c
