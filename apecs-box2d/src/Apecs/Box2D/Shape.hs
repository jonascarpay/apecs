{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- 'Shape' and 'Chain' themselves live in "Apecs.Box2D.Types" — the
-- store's registries embed them — so their instances here are orphans.
{-# OPTIONS_GHC -Wno-orphans #-}

{-| The 'Shape' and 'Chain' components, the engine shape lifecycle
behind them, and the shape sub-components ('Density', 'Friction', ...).
-}
module Apecs.Box2D.Shape where

import Apecs
import Apecs.Core
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Marshal.Utils (fromBool)

import Box2D.Chain qualified as B2Chain
import Box2D.Id (BodyId, ChainId, ShapeId (..))
import Box2D.Shape (Filter)
import Box2D.Shape qualified as B2Shape
import Box2D.UserData (setUserIndex)

import Apecs.Box2D.Geometry
import Apecs.Box2D.Types

instance Component Shape where
  type Storage Shape = B2Space Shape

instance (MonadIO m, Has w m Physics) => Has w m Shape where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

{- | A shape def with the surface material, density, filter and sensor
flag carried over from the shape being replaced, if any.
-}
carryMaterial :: B2Shape.ShapeDef -> Maybe ShapeRecord -> IO B2Shape.ShapeDef
carryMaterial sd Nothing = pure sd
carryMaterial sd (Just (ShapeRecord s _)) = do
  material <- B2Shape.getSurfaceMaterial s
  density <- B2Shape.getDensity s
  filtr <- B2Shape.getFilter s
  sensor <- B2Shape.isSensor s
  pure
    sd
      { B2Shape.material = material
      , B2Shape.density = density
      , B2Shape.filter = filtr
      , B2Shape.isSensor = fromBool sensor
      }

{- | Create a fresh engine shape for a 'Shape' value with the given def,
tag it with the entity's user index, destroy the shape it replaces (if
any) only after the new one exists (so a failed create, e.g. a bad
polygon, leaves everything intact), and update the shape registry.
Shared by 'Shape' and 'Sensor', which both recreate the shape while
preserving its material state.
-}
recreateShape :: B2Space c -> BodyId -> Int -> B2Shape.ShapeDef -> Shape -> Maybe ShapeRecord -> IO ()
recreateShape sp b ety sd shape@(Shape (Entity bEty) geo) old = do
  s <- createGeometry b sd geo
  setUserIndex s ety
  forM_ old $ \(ShapeRecord s' (Shape (Entity oldBEty) _)) -> do
    B2Shape.destroy s' True
    when (oldBEty /= bEty) $ depDelete sp.shapesByBody oldBEty ety
  depInsert sp.shapesByBody bEty ety
  modifyIORef' sp.shapes (IM.insert ety (ShapeRecord s shape))

instance (MonadIO m) => ExplSet m (B2Space Shape) where
  explSet sp ety shape@(Shape (Entity bEty) _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef sp.shapes
      sd <- carryMaterial sp.shapeDef old
      recreateShape sp b ety sd shape old

instance (MonadIO m) => ExplGet m (B2Space Shape) where
  explExists = shapeExists
  explGet sp ety = liftIO $
    withReg "Shape" sp.shapes ety $
      \(ShapeRecord _ shape) -> pure shape

instance (MonadIO m) => ExplDestroy m (B2Space Shape) where
  explDestroy sp ety = liftIO $ do
    shapes <- readIORef sp.shapes
    forM_ (IM.lookup ety shapes) $ \(ShapeRecord s (Shape (Entity bEty) _)) -> do
      modifyIORef' sp.shapes (IM.delete ety)
      depDelete sp.shapesByBody bEty ety
      B2Shape.destroy s True

instance (MonadIO m) => ExplMembers m (B2Space Shape) where
  explMembers = shapeMembers

-- | The raw Box2D shape of an entity, for use with "Box2D.Shape" directly.
newtype B2ShapeId = B2ShapeId ShapeId
  deriving (Eq, Show)

instance Component B2ShapeId where
  type Storage B2ShapeId = B2Space B2ShapeId

instance (MonadIO m, Has w m Physics) => Has w m B2ShapeId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2ShapeId) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety (pure . B2ShapeId)

instance (MonadIO m) => ExplMembers m (B2Space B2ShapeId) where
  explMembers = shapeMembers

{- | The fewest points the engine accepts for a chain (both open and
looped), per @b2ChainDef@: it uses the extra points as ghost vertices
to suppress ghost collisions at internal joints.
-}
minChainPoints :: Int
minChainPoints = 4

instance Component Chain where
  type Storage Chain = B2Space Chain

instance (MonadIO m, Has w m Physics) => Has w m Chain where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

{- | Fetch a chain's segment shape ids, in creation order — the two-call
@b2Chain_GetSegmentCount@\/@b2Chain_GetSegments@ dance the raw binding
exposes directly.
-}
chainSegments :: ChainId -> IO (VS.Vector ShapeId)
chainSegments c = do
  n <- B2Chain.getSegmentCount c
  buf <- VSM.new n
  got <- VSM.unsafeWith buf $ \arr -> B2Chain.getSegments c arr n
  VS.take got <$> VS.unsafeFreeze buf

{- | Create a fresh engine chain for a 'Chain' value, destroy the chain it
replaces (if any) only after the new one exists (so a failed create,
e.g. too few points, leaves everything intact), and update the chain
registry. Mirrors 'recreateShape'.
-}
recreateChain :: B2Space c -> BodyId -> Int -> Chain -> Maybe ChainRecord -> IO ()
recreateChain sp b ety chain@(Chain (Entity bEty) pts isLoop) old = do
  let n = VS.length pts
  when (n < minChainPoints) $
    error ("chain needs at least " <> show minChainPoints <> " points, got " <> show n)
  def <- B2Shape.defaultChainDef
  c <- VS.unsafeWith pts $ \p ->
    B2Chain.create
      b
      def
        { B2Shape.points = p
        , B2Shape.count = fromIntegral n
        , B2Shape.isLoop = fromBool isLoop
        , -- like 'explInit'\'s sensor-events opt-in for layer-created
          -- shapes: lets sensors see this chain as a visitor
          B2Shape.enableSensorEvents = 1
        }
  -- Unlike BodyId/ShapeId/JointId, ChainId has no 'Box2D.UserData.HasUserData'
  -- instance, so the chain itself cannot be stamped with the entity's user
  -- index (the chain registry in "Apecs.Box2D.Types", keyed by entity, is what
  -- 'explGet'/'B2ChainId' use instead). Its segment 'ShapeId's are stamped
  -- with it instead, and 'shapeEntities' falls back to this registry when a
  -- shape isn't in the shape registry, matching the id against the segment
  -- ids recorded here.
  segs <- chainSegments c
  VS.forM_ segs $ \seg -> setUserIndex seg ety
  -- Chain creation turns segment contact/hit events off; opt every segment
  -- in the way 'explInit' does per-shape for layer-created 'Shape's.
  B2Chain.enableContactEvents c True
  B2Chain.enableHitEvents c True
  forM_ old $ \(ChainRecord c' _ (Chain (Entity oldBEty) _ _)) -> do
    B2Chain.destroy c'
    when (oldBEty /= bEty) $ depDelete sp.chainsByBody oldBEty ety
  depInsert sp.chainsByBody bEty ety
  let segSet = IS.fromList [fromIntegral w | ShapeId w <- VS.toList segs]
  modifyIORef' sp.chains (IM.insert ety (ChainRecord c segSet chain))

instance (MonadIO m) => ExplSet m (B2Space Chain) where
  explSet sp ety chain@(Chain (Entity bEty) _ _) = liftIO $
    overBody sp bEty $ \b -> do
      old <- IM.lookup ety <$> readIORef sp.chains
      recreateChain sp b ety chain old

instance (MonadIO m) => ExplGet m (B2Space Chain) where
  explExists = chainExists
  explGet sp ety = liftIO $
    withReg "Chain" sp.chains ety $
      \(ChainRecord _ _ chain) -> pure chain

instance (MonadIO m) => ExplDestroy m (B2Space Chain) where
  explDestroy sp ety = liftIO $ do
    chains <- readIORef sp.chains
    forM_ (IM.lookup ety chains) $ \(ChainRecord c _ (Chain (Entity bEty) _ _)) -> do
      modifyIORef' sp.chains (IM.delete ety)
      depDelete sp.chainsByBody bEty ety
      B2Chain.destroy c

instance (MonadIO m) => ExplMembers m (B2Space Chain) where
  explMembers = chainMembers

-- | The raw Box2D chain of an entity, for use with "Box2D.Chain" directly.
newtype B2ChainId = B2ChainId ChainId
  deriving (Eq, Show)

instance Component B2ChainId where
  type Storage B2ChainId = B2Space B2ChainId

instance (MonadIO m, Has w m Physics) => Has w m B2ChainId where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space B2ChainId) where
  explExists = chainExists
  explGet sp ety = liftIO $ withChain sp ety (pure . B2ChainId)

instance (MonadIO m) => ExplMembers m (B2Space B2ChainId) where
  explMembers = chainMembers

-- Shape sub-components -----------------------------------------------------

-- | The density of a 'Shape'. Setting it updates the body's mass.
newtype Density = Density Float
  deriving (Eq, Show)

instance Component Density where
  type Storage Density = B2Space Density

instance (MonadIO m, Has w m Physics) => Has w m Density where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Density) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Density . B2Shape.getDensity

instance (MonadIO m) => ExplSet m (B2Space Density) where
  explSet sp ety (Density d) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setDensity s d True

instance (MonadIO m) => ExplMembers m (B2Space Density) where
  explMembers = shapeMembers

-- | The friction coefficient of a 'Shape'.
newtype Friction = Friction Float
  deriving (Eq, Show)

instance Component Friction where
  type Storage Friction = B2Space Friction

instance (MonadIO m, Has w m Physics) => Has w m Friction where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Friction) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Friction . B2Shape.getFriction

instance (MonadIO m) => ExplSet m (B2Space Friction) where
  explSet sp ety (Friction f) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setFriction s f

instance (MonadIO m) => ExplMembers m (B2Space Friction) where
  explMembers = shapeMembers

-- | The elasticity of a 'Shape' (Box2D calls this restitution).
newtype Elasticity = Elasticity Float
  deriving (Eq, Show)

instance Component Elasticity where
  type Storage Elasticity = B2Space Elasticity

instance (MonadIO m, Has w m Physics) => Has w m Elasticity where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Elasticity) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Elasticity . B2Shape.getRestitution

instance (MonadIO m) => ExplSet m (B2Space Elasticity) where
  explSet sp ety (Elasticity e) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setRestitution s e

instance (MonadIO m) => ExplMembers m (B2Space Elasticity) where
  explMembers = shapeMembers

-- | The collision 'Filter' of a 'Shape' (category, mask, group).
newtype CollisionFilter = CollisionFilter Filter
  deriving (Eq, Show)

instance Component CollisionFilter where
  type Storage CollisionFilter = B2Space CollisionFilter

instance (MonadIO m, Has w m Physics) => Has w m CollisionFilter where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space CollisionFilter) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap CollisionFilter . B2Shape.getFilter

instance (MonadIO m) => ExplSet m (B2Space CollisionFilter) where
  explSet sp ety (CollisionFilter f) = liftIO $
    overShape sp ety $
      \s -> B2Shape.setFilter s f

instance (MonadIO m) => ExplMembers m (B2Space CollisionFilter) where
  explMembers = shapeMembers

{- | Whether a 'Shape' is a sensor: a trigger volume that reports
overlaps through 'Apecs.Box2D.Collision.SensorEvents' instead of generating contacts. Box2D
cannot change a live shape from sensor to solid or back, so setting
this recreates the engine shape (as 'Shape' does, preserving 'Density',
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
  type Storage Sensor = B2Space Sensor

instance (MonadIO m, Has w m Physics) => Has w m Sensor where
  getStore = cast <$> (getStore :: SystemT w m (B2Space Physics))

instance (MonadIO m) => ExplGet m (B2Space Sensor) where
  explExists = shapeExists
  explGet sp ety = liftIO $ withShape sp ety $ fmap Sensor . B2Shape.isSensor

instance (MonadIO m) => ExplSet m (B2Space Sensor) where
  explSet sp ety (Sensor wantSensor) = liftIO $ do
    old <- IM.lookup ety <$> readIORef sp.shapes
    forM_ old $ \old'@(ShapeRecord s shape@(Shape (Entity bEty) _)) -> do
      isSensorNow <- B2Shape.isSensor s
      when (isSensorNow /= wantSensor) $
        overBody sp bEty $ \b -> do
          sd <- carryMaterial sp.shapeDef (Just old')
          recreateShape sp b ety sd{B2Shape.isSensor = fromBool wantSensor} shape (Just old')

instance (MonadIO m) => ExplMembers m (B2Space Sensor) where
  explMembers = shapeMembers
