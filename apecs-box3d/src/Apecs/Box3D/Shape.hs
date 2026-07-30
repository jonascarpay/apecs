{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'Shape' itself lives in "Apecs.Box3D.Types" — the store's shape
-- registry embeds it — so its instances here are orphans.
{-# OPTIONS_GHC -Wno-orphans #-}

{-| The 'Shape' component, the engine shape lifecycle behind it, and
the shape sub-components ('Density', 'Friction', ...).
-}
module Apecs.Box3D.Shape where

import Apecs
import Apecs.Core
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Foreign.Marshal.Utils (fromBool)

import Box3D.Id (BodyId, ShapeId)
import Box3D.Shape (Filter)
import Box3D.Shape qualified as B3Shape
import Box3D.UserData (setUserIndex)

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

instance Component Shape where
  type Storage Shape = B3Space Shape

instance (MonadIO m, Has w m Physics) => Has w m Shape where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

{- | A shape def with the surface material, density, filter and sensor
flag carried over from the shape being replaced, if any.
-}
carryMaterial :: B3Shape.ShapeDef -> Maybe ShapeRecord -> IO B3Shape.ShapeDef
carryMaterial sd Nothing = pure sd
carryMaterial sd (Just (ShapeRecord s _)) = do
  material <- B3Shape.getSurfaceMaterial s
  density <- B3Shape.getDensity s
  filtr <- B3Shape.getFilter s
  sensor <- B3Shape.isSensor s
  pure
    sd
      { B3Shape.baseMaterial = material
      , B3Shape.density = density
      , B3Shape.filter = filtr
      , B3Shape.isSensor = fromBool sensor
      }

{- | Create a fresh engine shape for a 'Shape' value with the given def,
tag it with the entity's user index, destroy the shape it replaces (if
any) only after the new one exists (so a failed create, e.g. a bad
hull, leaves everything intact), and update the shape registry. Shared
by 'Shape' and 'Sensor', which both recreate the shape while preserving
its material state.
-}
recreateShape :: B3Space c -> BodyId -> Int -> B3Shape.ShapeDef -> Shape -> Maybe ShapeRecord -> IO ()
recreateShape sp b ety sd shape@(Shape (Entity bEty) geo) old = do
  s <- createGeometry b sd geo
  setUserIndex s ety
  forM_ old $ \(ShapeRecord s' (Shape (Entity oldBEty) _)) -> do
    B3Shape.destroy s' True
    when (oldBEty /= bEty) $ depDelete sp.shapesByBody oldBEty ety
  depInsert sp.shapesByBody bEty ety
  modifyIORef' sp.shapes (IM.insert ety (ShapeRecord s shape))

instance (MonadIO m) => ExplSet m (B3Space Shape) where
  explSet sp ety shape@(Shape (Entity bEty) _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef sp.shapes
      sd <- carryMaterial sp.shapeDef old
      recreateShape sp b ety sd shape old

instance (MonadIO m) => ExplGet m (B3Space Shape) where
  explExists = shapeExists
  explGet sp ety = liftIO $
    withReg "Shape" sp.shapes ety $
      \(ShapeRecord _ shape) -> pure shape

instance (MonadIO m) => ExplDestroy m (B3Space Shape) where
  explDestroy sp ety = liftIO $ do
    shapes <- readIORef sp.shapes
    forM_ (IM.lookup ety shapes) $ \(ShapeRecord s (Shape (Entity bEty) _)) -> do
      -- destroy the engine shape before dropping the record: the record
      -- keeps mesh and height-field ForeignPtrs alive, and the engine
      -- shape references that geometry until it is destroyed
      B3Shape.destroy s True
      modifyIORef' sp.shapes (IM.delete ety)
      depDelete sp.shapesByBody bEty ety

instance (MonadIO m) => ExplMembers m (B3Space Shape) where
  explMembers = shapeMembers

-- | The raw Box3D shape of an entity, for use with "Box3D.Shape" directly.
newtype B3ShapeId = B3ShapeId ShapeId
  deriving (Eq, Show)

instance Component B3ShapeId where
  type Storage B3ShapeId = B3Space B3ShapeId

instance (MonadIO m, Has w m Physics) => Has w m B3ShapeId where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space B3ShapeId) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety (pure . B3ShapeId)

instance (MonadIO m) => ExplMembers m (B3Space B3ShapeId) where
  explMembers = shapeMembers

-- Shape sub-components -----------------------------------------------------

-- | The density of a 'Shape'. Setting it updates the body's mass.
newtype Density = Density Float
  deriving (Eq, Show)

instance Component Density where
  type Storage Density = B3Space Density

instance (MonadIO m, Has w m Physics) => Has w m Density where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Density) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Density . B3Shape.getDensity

instance (MonadIO m) => ExplSet m (B3Space Density) where
  explSet sp ety (Density d) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setDensity s d True

instance (MonadIO m) => ExplMembers m (B3Space Density) where
  explMembers = shapeMembers

-- | The friction coefficient of a 'Shape'.
newtype Friction = Friction Float
  deriving (Eq, Show)

instance Component Friction where
  type Storage Friction = B3Space Friction

instance (MonadIO m, Has w m Physics) => Has w m Friction where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Friction) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Friction . B3Shape.getFriction

instance (MonadIO m) => ExplSet m (B3Space Friction) where
  explSet sp ety (Friction f) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setFriction s f

instance (MonadIO m) => ExplMembers m (B3Space Friction) where
  explMembers = shapeMembers

-- | The elasticity of a 'Shape' (Box3D calls this restitution).
newtype Elasticity = Elasticity Float
  deriving (Eq, Show)

instance Component Elasticity where
  type Storage Elasticity = B3Space Elasticity

instance (MonadIO m, Has w m Physics) => Has w m Elasticity where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Elasticity) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Elasticity . B3Shape.getRestitution

instance (MonadIO m) => ExplSet m (B3Space Elasticity) where
  explSet sp ety (Elasticity e) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setRestitution s e

instance (MonadIO m) => ExplMembers m (B3Space Elasticity) where
  explMembers = shapeMembers

-- | The collision 'Filter' of a 'Shape' (category, mask, group).
newtype CollisionFilter = CollisionFilter Filter
  deriving (Eq, Show)

instance Component CollisionFilter where
  type Storage CollisionFilter = B3Space CollisionFilter

instance (MonadIO m, Has w m Physics) => Has w m CollisionFilter where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space CollisionFilter) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap CollisionFilter . B3Shape.getFilter

instance (MonadIO m) => ExplSet m (B3Space CollisionFilter) where
  explSet sp ety (CollisionFilter f) = liftIO $
    overShape sp ety $
      \s -> B3Shape.setFilter s f True

instance (MonadIO m) => ExplMembers m (B3Space CollisionFilter) where
  explMembers = shapeMembers

{- | Whether a 'Shape' is a sensor: a trigger volume that reports
overlaps through 'Apecs.Box3D.Collision.SensorEvents' instead of generating contacts. Box3D
has no way to change a live shape's sensor flag, so setting this
recreates the engine shape (as 'Shape' does, preserving 'Density',
'Friction', 'Elasticity' and 'CollisionFilter') whenever the requested
value differs from the shape's current one; setting the value it
already has is a no-op. Re-setting 'Shape' preserves the sensor flag
the same way. Reads reflect the engine. Setting it on an entity that
has no 'Shape' yet is a silent no-op, so in a 'newEntity' tuple put
'Shape' before 'Sensor' — components are set left to right.
-}
newtype Sensor = Sensor Bool
  deriving (Eq, Show)

instance Component Sensor where
  type Storage Sensor = B3Space Sensor

instance (MonadIO m, Has w m Physics) => Has w m Sensor where
  getStore = cast <$> (getStore :: SystemT w m (B3Space Physics))

instance (MonadIO m) => ExplGet m (B3Space Sensor) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Sensor . B3Shape.isSensor

instance (MonadIO m) => ExplSet m (B3Space Sensor) where
  explSet sp ety (Sensor wantSensor) = liftIO $ do
    old <- IM.lookup ety <$> readIORef sp.shapes
    forM_ old $ \old'@(ShapeRecord s shape@(Shape (Entity bEty) _)) -> do
      isSensorNow <- B3Shape.isSensor s
      when (isSensorNow /= wantSensor) $
        overBody sp bEty $ \b -> do
          sd <- carryMaterial sp.shapeDef (Just old')
          recreateShape sp b ety sd{B3Shape.isSensor = fromBool wantSensor} shape (Just old')

instance (MonadIO m) => ExplMembers m (B3Space Sensor) where
  explMembers = shapeMembers
