{-# LANGUAGE OverloadedRecordDot #-}

{-| Shape geometry in body-local coordinates ('Geometry'), the engine
geometry it creates, and the body-space\/world-space vector aliases.
-}
module Apecs.Box2D.Geometry where

import Control.Monad (when)
import Data.Vector.Storable qualified as VS

import Box2D.Collision qualified as B2Collision
import Box2D.Geometry qualified as B2Geometry
import Box2D.Id (BodyId, ShapeId)
import Box2D.MathFunctions (makeRot)
import Box2D.MathTypes (Vec2 (..))
import Box2D.Shape qualified as B2Shape

-- | A vector in body-space coordinates.
type BVec = Vec2

-- | A vector in world-space coordinates.
type WVec = Vec2

-- | Shape geometry in body-local coordinates.
data Geometry
  = -- | Center and radius.
    GeoCircle BVec Float
  | -- | The two centers and the radius around the segment between them.
    GeoCapsule BVec BVec Float
  | -- | A two-sided line segment.
    GeoSegment BVec BVec
  | -- | An axis-aligned box from half-width and half-height.
    GeoBox Float Float
  | -- | Half-width, half-height, corner radius: a box with rounded corners.
    GeoRoundedBox Float Float Float
  | {- | Half-width, half-height, local center, local rotation angle
    (radians): a box placed off the body origin.
    -}
    GeoOffsetBox Float Float BVec Float
  | {- | The convex hull of 3 to 'B2Geometry.maxPolygonVertices' points. Setting
    an out-of-range or degenerate (collinear) point set raises an error.
    -}
    GeoPolygon (VS.Vector Vec2)
  | {- | The convex hull of 3 to 'B2Geometry.maxPolygonVertices' points, placed
    off the body origin at a local center and rotation angle (radians)
    and rounded by the given corner radius. Setting an out-of-range or
    degenerate (collinear) point set raises an error.
    -}
    GeoOffsetRoundedPolygon (VS.Vector Vec2) BVec Float Float
  deriving (Eq, Show)

{- | Compute a validated convex hull for a polygon-shaped 'Geometry',
shared by 'GeoPolygon' and 'GeoOffsetRoundedPolygon'.
-}
computeValidHull :: VS.Vector Vec2 -> IO B2Geometry.Hull
computeValidHull pts = do
  let n = VS.length pts
  when (n < 3 || n > B2Geometry.maxPolygonVertices) $
    error ("polygon needs 3 to " <> show B2Geometry.maxPolygonVertices <> " points, got " <> show n)
  hull <- VS.unsafeWith pts $ \p -> B2Collision.computeHull p n
  when (VS.length hull.points < 3) $
    error "polygon points are degenerate (collinear or coincident)"
  pure hull

{- | The engine polygon behind a polygon-backed 'Geometry' constructor —
one dispatch shared by 'createGeometry' and 'geometryProxy', so the
shape a 'Geometry' creates and the proxy it queries with can never
drift. Errors on the point-backed constructors (circle, capsule,
segment), which both callers handle themselves.
-}
geometryPolygon :: Geometry -> IO B2Geometry.Polygon
geometryPolygon geo = case geo of
  GeoBox hw hh -> B2Collision.makeBox hw hh
  GeoRoundedBox hw hh r -> B2Collision.makeRoundedBox hw hh r
  GeoOffsetBox hw hh center angle -> do
    rot <- makeRot angle
    B2Collision.makeOffsetBox hw hh center rot
  GeoPolygon pts -> do
    hull <- computeValidHull pts
    B2Collision.makePolygon hull 0
  GeoOffsetRoundedPolygon pts center angle r -> do
    hull <- computeValidHull pts
    rot <- makeRot angle
    B2Collision.makeOffsetRoundedPolygon hull center rot r
  GeoCircle{} -> notPolygon
  GeoCapsule{} -> notPolygon
  GeoSegment{} -> notPolygon
  where
    notPolygon = error "geometryPolygon: not a polygon-backed Geometry"

-- | Create the engine geometry for a 'Geometry' value on a body.
createGeometry :: BodyId -> B2Shape.ShapeDef -> Geometry -> IO ShapeId
createGeometry b sd geo = case geo of
  GeoCircle c r -> B2Shape.createCircle b sd (B2Geometry.Circle c r)
  GeoCapsule c1 c2 r -> B2Shape.createCapsule b sd (B2Geometry.Capsule c1 c2 r)
  GeoSegment p1 p2 -> B2Shape.createSegment b sd (B2Geometry.Segment p1 p2)
  _ -> geometryPolygon geo >>= B2Shape.createPolygon b sd

{- | A query 'Geometry', interpreted in world space, as a shape proxy
point cloud and radius, for 'Apecs.Box2D.Query.overlapQuery' and 'Apecs.Box2D.Query.sweepQuery'.
'GeoCircle', 'GeoCapsule' and 'GeoSegment' hand over their defining
points directly, circle and capsule carrying their radius onto the
proxy (their created shapes place the same points verbatim); every
other constructor reads the proxy off the engine polygon
'geometryPolygon' builds — the same dispatch 'createGeometry' uses —
so a query with the same 'Geometry' as a created shape tests exactly
the created volume, including 'computeValidHull' rejecting degenerate
polygon point sets the same way shape creation does.
-}
geometryProxy :: Geometry -> IO (VS.Vector Vec2, Float)
geometryProxy geo = case geo of
  GeoCircle c r -> pure (VS.singleton c, r)
  GeoCapsule c1 c2 r -> pure (VS.fromListN 2 [c1, c2], r)
  GeoSegment p1 p2 -> pure (VS.fromListN 2 [p1, p2], 0)
  _ -> do
    poly <- geometryPolygon geo
    pure (poly.vertices, poly.radius)
