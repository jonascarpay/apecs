{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | World queries: ray and shape casts, AABB, point and overlap tests.
module Apecs.Box3D.Query where

import Apecs
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.IntSet qualified as IS
import Data.List (sortOn)
import Foreign.Ptr (FunPtr, Ptr)

import Box3D.Callbacks (withCastResultFcn, withOverlapResultFcn)
import Box3D.Extra (withShapeProxy)
import Box3D.Id (ShapeId)
import Box3D.MathTypes (AABB (..), Vec3 (..), vec3Sub, vec3Zero)
import Box3D.Query qualified as B3Query
import Box3D.Shape (Filter (..))
import Box3D.Shape qualified as B3Shape
import Box3D.Tags qualified as B3Tags
import Box3D.World qualified as B3World

import Apecs.Box3D.Geometry
import Apecs.Box3D.Types

{- | The closest shape a 'segmentQuery' found: the shape entity, the
body entity it hangs off, the world-space impact point and surface
normal, and the fraction along the segment (0 at the start, 1 at the
end).
-}
data RayHit = RayHit
  { shape :: !Entity
  , body :: !Entity
  , point :: !WVec
  , normal :: !WVec
  , fraction :: !Float
  }
  deriving (Eq, Show)

{- | The query filter that matches every shape: all category and mask
bits set — the "just give me everything" argument for 'aabbQuery',
'containsPointQuery' and friends.
-}
everything :: Filter
everything =
  Filter
    { categoryBits = maxBound
    , maskBits = maxBound
    , groupIndex = 0
    }

{- | Queries match a 'Filter''s category and mask bits against shape
filters (see 'Apecs.Box3D.Shape.CollisionFilter'); 'filterGroupIndex' does not apply.
'everything' queries everything. Note Box3D's default shape category
is /all bits set/ (unlike Box2D's category 1), so any query mask
matches shapes with default filters — give shapes explicit
'Apecs.Box3D.Shape.CollisionFilter' categories to partition them for queries.
-}
toQueryFilter :: Filter -> IO B3Query.QueryFilter
toQueryFilter f = do
  qf <- B3Query.defaultQueryFilter
  pure
    qf
      { B3Query.categoryBits = f.categoryBits
      , B3Query.maskBits = f.maskBits
      }

{- | The closest shape along a world-space segment, if any. Initial
overlaps are ignored: a segment starting inside a shape does not hit
it.
-}
segmentQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m (Maybe RayHit)
segmentQuery start end fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let
      Vec3 sx sy sz = start
      Vec3 ex ey ez = end
    res <- B3World.castRayClosest sp.world start (Vec3 (ex - sx) (ey - sy) (ez - sz)) qf
    if res.hit == 0 then
      pure Nothing
    else
      fmap
        ( \(shapeEty, bodyEty) ->
            RayHit
              { shape = shapeEty
              , body = bodyEty
              , point = res.point
              , normal = res.normal
              , fraction = res.fraction
              }
        )
        <$> shapeEntities sp res.shapeId

{- | Drive a cast-style engine query ('B3World.castRay') with the
collect-everything visitor: each reported shape becomes a 'RayHit'
(hits whose shapes were destroyed since the last 'Apecs.Box3D.Space.stepPhysics' are
dropped), sorted nearest-first by 'fraction'.
-}
collectCastHits :: B3Space c -> (FunPtr B3Tags.CastResultFcn -> Ptr () -> IO r) -> IO [RayHit]
collectCastHits sp run = do
  shapes <- readIORef sp.shapes
  found <- newIORef []
  let visit s point normal frac _matId _triangleIx _childIx = do
        hit <- shapeEntitiesIn shapes s
        forM_ hit $ \(shapeEty, bodyEty) ->
          modifyIORef'
            found
            ( RayHit
                { shape = shapeEty
                , body = bodyEty
                , point = point
                , normal = normal
                , fraction = frac
                }
                :
            )
        pure 1
  _ <- withCastResultFcn visit run
  sortOn (.fraction) <$> readIORef found

{- | Drive an overlap-style engine query ('B3World.overlapAABB'),
collecting the deduplicated body entities of every reported shape that
passes the keep test. The keep test runs before entity resolution: it
is at most one FFI call while resolution is two plus a registry
lookup, and (for the exact tests) most broad-phase candidates fail it.
-}
collectOverlapBodies
  :: B3Space c
  -> (ShapeId -> IO Bool)
  -> (FunPtr B3Tags.OverlapResultFcn -> Ptr () -> IO r)
  -> IO [Entity]
collectOverlapBodies sp keep run = do
  shapes <- readIORef sp.shapes
  found <- newIORef IS.empty
  let visit s = do
        wanted <- keep s
        when wanted $ do
          hit <- shapeEntitiesIn shapes s
          forM_ hit $ \(_, Entity bodyIx) -> modifyIORef' found (IS.insert bodyIx)
        pure True
  _ <- withOverlapResultFcn visit run
  map Entity . IS.toList <$> readIORef found

{- | Every shape along a world-space segment, sorted nearest-first by
'fraction'. Filter semantics match 'segmentQuery'. Unlike
'segmentQuery', which goes through the engine's @b3World_CastRayClosest@
convenience path, this drives the general @b3World_CastRay@ callback
directly — and that path does /not/ ignore initial overlaps itself (the
"ignore initial overlap" behaviour lives in the closest-hit callback, which
skips fraction-0 hits before they reach the caller). So a segment starting
inside a shape here reports that shape too, with 'fraction' 0. Hits
whose shapes were destroyed since the last 'Apecs.Box3D.Space.stepPhysics' are dropped, same
as 'segmentQuery'.
-}
segmentQueryAll
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m [RayHit]
segmentQueryAll start end fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    collectCastHits sp $ \fp ctx ->
      B3World.castRay sp.world start (vec3Sub end start) qf fp ctx

{- | The body entities whose shapes' broad-phase bounding boxes overlap
the world-space box spanned by two corners (any order). Broad-phase:
the test is against shape AABBs, not exact geometry.
-}
aabbQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> WVec
  -> Filter
  -> SystemT w m [Entity]
aabbQuery cornerA cornerB fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    collectOverlapBodies sp (\_ -> pure True) $ \fp ctx ->
      B3World.overlapAABB sp.world box qf fp ctx
  where
    Vec3 ax ay az = cornerA
    Vec3 bx by bz = cornerB
    box = AABB (Vec3 (min ax bx) (min ay by) (min az bz)) (Vec3 (max ax bx) (max ay by) (max az bz))

{- | 'aabbQuery' of the cube reaching @r@ along each axis from a point:
the body entities with shapes broad-phase within reach. This is
broad-phase AABB reach, /not/ exact containment — a shape's AABB is
larger than the shape itself, so this can return bodies whose shape
doesn't actually contain the point. See 'containsPointQuery' for the
exact test.
-}
pointQuery :: (MonadIO m, Has w m Physics) => WVec -> Float -> Filter -> SystemT w m [Entity]
pointQuery (Vec3 x y z) r =
  aabbQuery (Vec3 (x - r) (y - r) (z - r)) (Vec3 (x + r) (y + r) (z + r))

{- | The body entities with a shape that actually contains a world point:
an exact geometry test, unlike the broad-phase 'pointQuery'. Candidates
come from a broad-phase 'B3World.overlapAABB' at a degenerate (zero-size)
AABB pinned to the point, and each candidate shape is then refined with
'B3Shape.testPoint', an exact point-in-shape test; bodies are
deduplicated when more than one of their shapes contains the point.
Non-convex shapes — meshes, height fields and compounds — report no
containment, per 'B3Shape.testPoint's documented behavior.
-}
containsPointQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => WVec
  -> Filter
  -> SystemT w m [Entity]
containsPointQuery point fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    collectOverlapBodies sp (`B3Shape.testPoint` point) $ \fp ctx ->
      B3World.overlapAABB sp.world (AABB point point) qf fp ctx

{- | The body entities whose shapes actually overlap a query shape, given
as a world-space 'Geometry' — exact narrow-phase overlap, the
shape-shaped big brother of 'containsPointQuery'. Only the point-backed
convex 'Geometry' constructors can be query shapes; see 'geometryProxy'.
Remember Box3D's all-bits default shape category (see 'toQueryFilter')
when partitioning shapes for queries.
-}
overlapQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => Geometry
  -> Filter
  -> SystemT w m [Entity]
overlapQuery geo fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let (points, radius) = geometryProxy geo
    collectOverlapBodies sp (\_ -> pure True) $ \fp ctx ->
      withShapeProxy points radius $ \proxy ->
        B3World.overlapShape sp.world vec3Zero proxy qf fp ctx

{- | Sweep a query shape (a world-space 'Geometry') along a translation
and collect everything it would hit, sorted nearest-first by
'fraction' — 'segmentQueryAll' with volume. A shape that already
overlaps the query shape at the start of the sweep is reported too, at
fraction 0 (matching upstream @b3World_CastShape@, which treats an
initial overlap as an immediate hit rather than skipping it). Query
shape restrictions match 'overlapQuery'.
-}
sweepQuery
  :: forall w m
   . (MonadIO m, Has w m Physics)
  => Geometry
  -> WVec
  -> Filter
  -> SystemT w m [RayHit]
sweepQuery geo translation fltr = do
  sp :: B3Space Physics <- getStore
  liftIO $ do
    qf <- toQueryFilter fltr
    let (points, radius) = geometryProxy geo
    collectCastHits sp $ \fp ctx ->
      withShapeProxy points radius $ \proxy ->
        B3World.castShape sp.world vec3Zero proxy translation qf fp ctx
