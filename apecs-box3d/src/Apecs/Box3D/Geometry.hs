{-# LANGUAGE OverloadedRecordDot #-}

{-| Shape geometry in body-local coordinates ('Geometry'), the engine
geometry it creates, static geometry authoring ('meshFromData',
'compoundFromChildren', the mesh\/height-field\/hull generators), and
the body-space\/world-space vector aliases.
-}
module Apecs.Box3D.Geometry where

import Control.Monad (forM_, unless, when)
import Data.Int (Int32)
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.Concurrent qualified as Concurrent
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (toBool, with, withMany)
import Foreign.Ptr (Ptr, nullPtr)

import Box3D.Authoring (HeightFieldOptions (..), MeshOptions (..))
import Box3D.Authoring qualified as B3Authoring
import Box3D.Body qualified as B3Body
import Box3D.BoxMesh qualified as B3BoxMesh
import Box3D.Compound (CompoundData)
import Box3D.Compound qualified as B3Compound
import Box3D.Cone qualified as B3Cone
import Box3D.Cylinder qualified as B3Cylinder
import Box3D.Geometry qualified as B3Geometry
import Box3D.Grid qualified as B3Grid
import Box3D.GridMesh qualified as B3GridMesh
import Box3D.HeightField (HeightFieldData)
import Box3D.HeightField qualified as B3HeightField
import Box3D.HollowBoxMesh qualified as B3HollowBoxMesh
import Box3D.Hull (HullData)
import Box3D.Hull qualified as B3Hull
import Box3D.Id (BodyId, ShapeId)
import Box3D.MathTypes (Transform (..), Vec3 (..))
import Box3D.Mesh (MeshData)
import Box3D.Mesh qualified as B3Mesh
import Box3D.PlatformMesh qualified as B3PlatformMesh
import Box3D.Rock qualified as B3Rock
import Box3D.Shape qualified as B3Shape
import Box3D.TorusMesh qualified as B3TorusMesh
import Box3D.Wave qualified as B3Wave
import Box3D.WaveMesh qualified as B3WaveMesh

-- | A vector in body-space coordinates.
type BVec = Vec3

-- | A vector in world-space coordinates.
type WVec = Vec3

{- | Wrap a freshly generated engine data pointer in a 'ForeignPtr' whose
finalizer destroys it, and hand it to GC: once the last 'Apecs.Box3D.Types.Shape' (and the
last user binding) referencing it is gone, the finalizer runs and frees
the engine-side data. Errors immediately, without allocating a
'ForeignPtr', if the generator rejected its parameters and returned a
null pointer.
-}
wrapGenerated :: String -> (Ptr a -> IO ()) -> IO (Ptr a) -> IO (ForeignPtr a)
wrapGenerated what destroyIt gen = do
  p <- gen
  when (p == nullPtr) $
    error (what <> ": the engine rejected the parameters (returned a null pointer)")
  Concurrent.newForeignPtr p (destroyIt p)

{- | Shared triangle-mesh collision data, produced by the mesh generators
('boxMesh', 'hollowBoxMesh', 'platformMesh', 'gridMesh', 'torusMesh',
'waveMesh'). A 'GeoMesh' shape references this data instead of cloning
it, so the same 'Mesh' can be shared between shapes at different
per-shape scales. The underlying engine mesh is destroyed automatically
once no 'Apecs.Box3D.Types.Shape' (and no user binding) references it any more.
-}
data Mesh
  = {- | The engine mesh data plus its per-shape material slot count, tracked
    here because the binding exposes no way to read it back off the pointer
    and 'compoundFromChildren' needs it to reject multi-material children
    before the engine's own assertion aborts the process. The count
    shadows engine formulas (mesh.c's max-index+1 and the grid clamp);
    a @materialCount@ accessor in box-nd (NOTES-upstream §D) would
    delete it and this reverts to a plain newtype.
    -}
    Mesh !(ForeignPtr MeshData) !Int
  deriving (Eq, Show)

{- | Shared height-field collision data, produced by 'gridHeightField' and
'waveHeightField'. A 'GeoHeightField' shape references this data
instead of cloning it, so the same 'HeightField' can be shared between
shapes. The underlying engine height field is destroyed automatically
once no 'Apecs.Box3D.Types.Shape' (and no user binding) references it any more.
-}
newtype HeightField = HeightField (ForeignPtr HeightFieldData)
  deriving (Eq, Show)

{- | A pre-built convex hull, produced by 'rockHull', 'coneHull' or
'cylinderHull'. Unlike mesh and height-field data, the engine clones a
hull into the shape at creation time, so the handle only needs to stay
alive until the shape referencing it (a 'GeoReadyHull') is created; the
underlying engine hull is destroyed automatically once no 'Apecs.Box3D.Types.Shape' (and
no user binding) references it any more.
-}
newtype Hull = Hull (ForeignPtr HullData)
  deriving (Eq, Show)

{- | Compound collision data built from a mix of children by
'compoundFromChildren' — several capsules, spheres, pre-built hulls and
shared meshes baked into one static-body shape. Unlike 'Mesh'\/'HeightField',
the engine fully clones every child's geometry into the compound's own
allocation (@b3CreateCompound@ deep-copies the hull and mesh byte blobs it is
given, not just the small definition structs), so a 'Compound' has no need
to keep its source 'Hull'\/'Mesh' handles alive — they only need to live
until 'compoundFromChildren' returns, same as 'GeoReadyHull'. The underlying
engine compound is destroyed automatically once no 'Apecs.Box3D.Types.Shape' (and no user
binding) references it any more, same as 'Mesh'.
-}
newtype Compound = Compound (ForeignPtr CompoundData)
  deriving (Eq, Show)

-- | Shape geometry in body-local coordinates.
data Geometry
  = -- | Center and radius.
    GeoSphere BVec Float
  | {- | The two hemisphere centers and the radius around the segment
    between them.
    -}
    GeoCapsule BVec BVec Float
  | {- | A box from a local center and half-extents along each axis.
    A zero half-extent makes the corners coplanar and raises an error;
    give flat geometry a small thickness.
    -}
    GeoBox BVec Vec3
  | {- | The convex hull of at least 4 points. Setting a degenerate
    (coplanar) point set raises an error.
    -}
    GeoHull (VS.Vector Vec3)
  | {- | A triangle mesh at a per-shape scale ('Vec3 1 1 1' for
    unscaled). Static bodies only: mesh contacts are only generated
    against static bodies, so attaching this to a dynamic or kinematic
    body creates a shape with no contacts. The engine does /not/ clone
    the mesh data the way it clones hulls — it keeps a reference to the
    'Mesh' for as long as the engine shape exists, which is exactly why
    'Mesh' is GC-lifetime managed; see its docs.
    -}
    GeoMesh Mesh Vec3
  | {- | A height field. Static bodies only, for the same reason as
    'GeoMesh', and likewise the engine references the 'HeightField'
    rather than cloning it.
    -}
    GeoHeightField HeightField
  | {- | A pre-built convex hull from 'rockHull', 'coneHull' or
    'cylinderHull'. Unlike 'GeoMesh'\/'GeoHeightField', the engine
    clones the hull data into the shape, so the 'Hull' handle only needs
    to live until the shape is created.
    -}
    GeoReadyHull Hull
  | {- | A compound of several child shapes baked by 'compoundFromChildren'
    into one collision shape. Static bodies only, same restriction as
    'GeoMesh'\/'GeoHeightField' — the engine hard-asserts
    (@b3CreateCompoundShape@ / @b3CreateShape@) that the body is static and
    that the shape is not a sensor, so 'createGeometry' checks both up
    front and raises a normal Haskell error instead of letting the engine
    abort the process. Like 'GeoMesh', the engine references the
    'Compound' rather than cloning it into the shape, which is exactly why
    'Compound' is GC-lifetime managed; see its docs.
    -}
    GeoCompound Compound
  deriving (Eq, Show)

{- | Build a hull shape from a point cloud; the engine clones the hull
data, so the intermediate hull is destroyed right after. The label
names the originating 'Geometry' constructor in errors.
-}
createHullShape :: String -> BodyId -> B3Shape.ShapeDef -> VS.Vector Vec3 -> IO ShapeId
createHullShape what b sd pts = do
  let n = VS.length pts
  when (n < 4) $
    error (what <> " needs at least 4 points, got " <> show n)
  hull <- VS.unsafeWith pts $ \p -> B3Hull.create p n n
  when (hull == nullPtr) $
    error (what <> " points are degenerate (coplanar or coincident)")
  s <- B3Shape.createHull b sd hull
  B3Hull.destroy hull
  pure s

boxCorners :: Vec3 -> Vec3 -> VS.Vector Vec3
boxCorners (Vec3 cx cy cz) (Vec3 hx hy hz) =
  VS.fromList
    [ Vec3 (cx + sx * hx) (cy + sy * hy) (cz + sz * hz)
    | sx <- [-1, 1]
    , sy <- [-1, 1]
    , sz <- [-1, 1]
    ]

-- | Create the engine geometry for a 'Geometry' value on a body.
createGeometry :: BodyId -> B3Shape.ShapeDef -> Geometry -> IO ShapeId
createGeometry b sd geo = case geo of
  GeoSphere c r -> B3Shape.createSphere b sd (B3Geometry.Sphere c r)
  GeoCapsule c1 c2 r -> B3Shape.createCapsule b sd (B3Geometry.Capsule c1 c2 r)
  GeoBox c half -> createHullShape "GeoBox" b sd (boxCorners c half)
  GeoHull pts -> createHullShape "GeoHull" b sd pts
  GeoMesh (Mesh fp _) scale -> withForeignPtr fp $ \p -> B3Shape.createMesh b sd p scale
  GeoHeightField (HeightField fp) -> withForeignPtr fp $ \p -> B3Shape.createHeightField b sd p
  GeoReadyHull (Hull fp) -> withForeignPtr fp $ \p -> B3Shape.createHull b sd p
  GeoCompound (Compound fp) -> do
    bodyType <- B3Body.getType b
    when (bodyType /= B3Body.StaticBody) $
      error "GeoCompound: compound shapes are only allowed on static bodies"
    when (toBool sd.isSensor) $
      error "GeoCompound: compound shapes cannot be sensors"
    withForeignPtr fp $ \p -> with sd $ \pSd -> B3Shape.createBakedCompound b pSd p

{- | A query 'Geometry', interpreted in world space, as a shape proxy
point cloud and radius, for 'Apecs.Box3D.Query.overlapQuery' and
'Apecs.Box3D.Query.sweepQuery'. Only the point-backed convex
constructors can be query shapes: sphere and capsule hand over their
defining points and carry their radius onto the proxy, box and hull
hand over their corner\/point clouds (the engine takes the convex hull
of the proxy points, so the queried volume matches the created shape).
The proxy uses the points directly without building an engine hull, so
a point set 'createGeometry' would reject as degenerate still queries
fine — it just spans a flat volume. 'GeoMesh', 'GeoHeightField',
'GeoReadyHull' and 'GeoCompound' raise an error: their geometry lives
in opaque engine blobs with no point cloud to borrow.
-}
geometryProxy :: Geometry -> (VS.Vector Vec3, Float)
geometryProxy geo = case geo of
  GeoSphere c r -> (VS.singleton c, r)
  GeoCapsule c1 c2 r -> (VS.fromListN 2 [c1, c2], r)
  GeoBox c half -> (boxCorners c half, 0)
  GeoHull pts -> (pts, 0)
  GeoMesh{} -> notConvex
  GeoHeightField{} -> notConvex
  GeoReadyHull{} -> notConvex
  GeoCompound{} -> notConvex
  where
    notConvex = error "geometryProxy: only sphere, capsule, box and hull can be query shapes"

-- Static geometry from user data --------------------------------------------

{- | Triangle-mesh collision data built from your own vertices and indices —
the level-loading path, where the procedural generators ('boxMesh',
'gridMesh', ...) are the test-scene path. Needs at least 3 vertices and
vertex indices grouped 3 at a time, one triangle each, wound the same
way as the generators' own meshes — an index count that is not a
multiple of 3, or an index outside the vertex range, is rejected here
rather than read out of bounds by the engine; an optional material
index per triangle indexes into a shape's per-shape material slots (see
'B3Shape.setMeshMaterial') and, if given, must have exactly one entry
per triangle — a mismatched count is treated as invalid input rather
than read out of bounds. 'MeshOptions' controls vertex welding, the
BVH split strategy and whether triangle adjacency is identified (see
the "internal edges" note on 'gridMesh'); start from
'defaultMeshOptions'. The engine clones the input data, so the vectors
can be reused or dropped right after this returns.

Degenerate (zero-area) triangles are silently dropped from the built
mesh; the second element of the result lists their indices into the
input triangle list (i.e. the index of a bad triangle is
@indices@\'s @3*i@\/@3*i+1@\/@3*i+2@), so a level pipeline can flag bad
source data instead of silently losing collision. Errors, naming this
function, if the engine rejects the parameters (a null pointer, e.g.
from a mismatched material count).
-}
meshFromData
  :: VS.Vector Vec3
  -- ^ Triangle vertices.
  -> VS.Vector Int32
  -- ^ Triangle vertex indices, 3 per triangle.
  -> Maybe (VS.Vector Word8)
  -- ^ Per-triangle material indices.
  -> MeshOptions
  -> IO (Mesh, VS.Vector Int32)
meshFromData vertices indices materials opts = do
  -- the engine reads @vertices.data[index]@ with no bounds information, so
  -- malformed indices must be caught here, not left to a heap over-read
  when (VS.length indices `rem` 3 /= 0) $
    error ("meshFromData: indices length must be a multiple of 3, got " <> show (VS.length indices))
  unless (VS.all (\i -> i >= 0 && fromIntegral i < VS.length vertices) indices) $
    error ("meshFromData: triangle index out of bounds for " <> show (VS.length vertices) <> " vertices")
  (p, degenerate) <- B3Authoring.createMeshFromData vertices indices materials opts
  fp <- wrapGenerated "meshFromData" B3Mesh.destroy (pure p)
  let materialCount = case materials of
        Just m | not (VS.null m) -> 1 + fromIntegral (VS.maximum m)
        _ -> 1
  pure (Mesh fp materialCount, degenerate)

{- | Height-field collision data built from your own grid samples — the
level-loading path, where 'gridHeightField'\/'waveHeightField' are the
test-scene path. Needs at least 2 grid lines per axis and row-major
@countX * countZ@ height values; an optional material index per grid
cell (exactly @(countX - 1) * (countZ - 1)@ entries), where @0xFF@
marks a hole shapes fall through, same as the procedural height
fields'. 'HeightFieldOptions' controls the height range used for
quantization (share it between adjacent tiles so they line up flush)
and winding; start from 'defaultHeightFieldOptions'. The engine
quantizes the heights into its own storage, so the input vectors can
be reused or dropped right after this returns. Errors, naming this
function, on mismatched grid\/vector sizes — the engine reads
@countX * countZ@ heights (and the per-cell materials) with no bounds
information, so mismatches must be caught here, not left to a heap
over-read — or if the engine rejects the parameters.
-}
heightFieldFromData
  :: VS.Vector Float
  -- ^ Grid point heights, row-major, @countX * countZ@ samples.
  -> Maybe (VS.Vector Word8)
  -- ^ Grid cell material indices.
  -> Vec3
  -- ^ Scale; all components must be positive.
  -> Int
  -- ^ Grid lines along the x-axis.
  -> Int
  -- ^ Grid lines along the z-axis.
  -> HeightFieldOptions
  -> IO HeightField
heightFieldFromData heights materials scale countX countZ opts = do
  when (countX < 2 || countZ < 2) $
    error ("heightFieldFromData: needs at least 2 grid lines per axis, got " <> show countX <> "x" <> show countZ)
  when (VS.length heights /= countX * countZ) $
    error ("heightFieldFromData: expected " <> show (countX * countZ) <> " height samples, got " <> show (VS.length heights))
  forM_ materials $ \m ->
    when (VS.length m /= (countX - 1) * (countZ - 1)) $
      error ("heightFieldFromData: expected " <> show ((countX - 1) * (countZ - 1)) <> " cell materials, got " <> show (VS.length m))
  HeightField
    <$> wrapGenerated
      "heightFieldFromData"
      B3HeightField.destroy
      (B3Authoring.createHeightFieldFromData heights materials scale countX countZ opts)

{- | A child of a 'compoundFromChildren' compound, entirely in the
compound's local space. Mirrors the upstream @b3Compound*Def@ child kinds:
a capsule and a sphere place their own geometry directly (like 'GeoCapsule'
and 'GeoSphere' — there is no separate placement field because the
capsule\/sphere centers already are the placement), while a hull or mesh is
placed by an explicit local 'Transform', matching 'CompoundHullDef'\/
'CompoundMeshDef'.

Every child def upstream also carries a per-child 'B3Shape.SurfaceMaterial',
but that knob is deliberately not exposed here: 'compoundFromChildren'
fills it with 'B3Shape.defaultSurfaceMaterial' for every child, and per-shape
material tuning stays where it already lives for every other 'Geometry'
constructor — on the 'Apecs.Box3D.Types.Shape'\'s wrapping components (surface material,
friction, restitution), not on the geometry itself. A 'CompoundMesh'
likewise gets exactly one material slot; a 'Mesh' built with more than one
per-triangle material (e.g. 'gridMesh' called with a material count above
one, or 'meshFromData' given per-triangle material indices above zero) is
not supported as a compound child, and 'compoundFromChildren' rejects it
with an error rather than letting the engine's own material-count
assertion abort the process.
-}
data CompoundChild
  = -- | Center and radius, as 'GeoSphere'.
    CompoundSphere BVec Float
  | -- | The two hemisphere centers and the radius, as 'GeoCapsule'.
    CompoundCapsule BVec BVec Float
  | {- | A pre-built convex hull ('rockHull', 'coneHull', 'cylinderHull')
    placed by a local transform. The compound clones the hull data (see
    'Compound'\'s docs), so — like 'GeoReadyHull' — the 'Hull' handle
    only needs to live until 'compoundFromChildren' returns.
    -}
    CompoundHull Hull Transform
  | {- | Shared mesh data (see 'GeoMesh'\/'boxMesh'\/'meshFromData' etc.)
    placed by a local transform and a per-child scale. Only
    single-material meshes are supported ('compoundFromChildren' errors
    on the rest); see the note above.
    -}
    CompoundMesh Mesh Transform Vec3
  deriving (Eq, Show)

{- | Build compound collision data (@b3CreateCompound@) from a mix of
children — see 'CompoundChild'. Errors, naming this function, on an empty
child list: the upstream engine has no valid empty-compound representation
(it asserts internally rather than returning @NULL@), so this checks first
rather than letting the engine abort the process. Also errors if the
engine itself rejects the parameters (a null pointer, e.g. too many
children). The 'Hull'\/'Mesh' handles referenced by 'CompoundHull'\/
'CompoundMesh' children only need to stay alive for the duration of this
call; the resulting 'Compound' clones everything, as its docs explain.
-}
compoundFromChildren :: [CompoundChild] -> IO Compound
compoundFromChildren children = do
  when (null children) $
    error "compoundFromChildren: empty child list"
  -- the compound child def has one material slot; a multi-material mesh
  -- would trip the engine's material-count assertion and abort the process
  forM_ [mats | CompoundMesh (Mesh _ mats) _ _ <- children] $ \mats ->
    when (mats /= 1) $
      error ("compoundFromChildren: a 'CompoundMesh' child has " <> show mats <> " material slots; only single-material meshes are supported")
  material <- B3Shape.defaultSurfaceMaterial
  let
    capsules =
      VS.fromList
        [ B3Authoring.CompoundCapsuleDef (B3Geometry.Capsule c1 c2 r) material
        | CompoundCapsule c1 c2 r <- children
        ]
    spheres =
      VS.fromList
        [ B3Authoring.CompoundSphereDef (B3Geometry.Sphere c r) material
        | CompoundSphere c r <- children
        ]
    hullChildren = [(fp, tr) | CompoundHull (Hull fp) tr <- children]
    meshChildren = [(fp, tr, scale) | CompoundMesh (Mesh fp _) tr scale <- children]
  withMany withForeignPtr (map fst hullChildren) $ \hullPtrs ->
    withMany withForeignPtr (map (\(fp, _, _) -> fp) meshChildren) $ \meshPtrs ->
      with material $ \pMaterial -> do
        let
          hulls =
            VS.fromList
              [ B3Authoring.CompoundHullDef hp tr material
              | ((_, tr), hp) <- zip hullChildren hullPtrs
              ]
          meshes =
            VS.fromList
              [ B3Authoring.CompoundMeshDef mp tr scale pMaterial 1
              | ((_, tr, scale), mp) <- zip meshChildren meshPtrs
              ]
        Compound
          <$> wrapGenerated
            "compoundFromChildren"
            B3Compound.destroy
            (B3Authoring.createCompoundFromData capsules hulls meshes spheres)

-- Static geometry generators ------------------------------------------------

{- | A solid box mesh: 12 triangles from a local center and half-extents
along each axis (matching 'GeoBox'\'s half-extent convention). Triangle
adjacency is always identified (see the "internal edges" note on
'gridMesh') since there is no legitimate reason to skip it for a shape
this small.
-}
boxMesh :: BVec -> Vec3 -> IO Mesh
boxMesh center halfExtents =
  (`Mesh` 1) <$> wrapGenerated "boxMesh" B3Mesh.destroy (B3BoxMesh.create center halfExtents True)

{- | A hollow box mesh: the same box as 'boxMesh' but with every triangle's
winding reversed, so its inside faces are solid and its outside is
open — a box-shaped room instead of a box-shaped solid.
-}
hollowBoxMesh :: BVec -> Vec3 -> IO Mesh
hollowBoxMesh center halfExtents =
  (`Mesh` 1) <$> wrapGenerated "hollowBoxMesh" B3Mesh.destroy (B3HollowBoxMesh.create center halfExtents)

{- | A platform mesh: a truncated pyramid (frustum) centered locally on
'center', with a 'topWidth' square face at +height\/2 and a
'bottomWidth' square face at -height\/2.
-}
platformMesh :: BVec -> Float -> Float -> Float -> IO Mesh
platformMesh center height topWidth bottomWidth =
  (`Mesh` 1) <$> wrapGenerated "platformMesh" B3Mesh.destroy (B3PlatformMesh.create center height topWidth bottomWidth)

{- | A flat grid mesh of @xCount * zCount@ cells (each 'cellWidth' wide) in
the local XZ plane, centered on the origin. 'materialCount' round-robins
the triangles across that many per-shape material slots for
'B3Shape.setMeshMaterial' (0 or 1 for a single material). Triangle
adjacency is always identified: this flags shared edges between
coplanar (or near-coplanar) triangles as non-colliding "internal
edges", which is what stops a ball or capsule from catching on the
seams between a mesh's triangles as it rolls across them. There is no
real use case for turning this off, so unlike the upstream C API this
binding does not expose the choice.
-}
gridMesh :: Int -> Int -> Float -> Int -> IO Mesh
gridMesh xCount zCount cellWidth materialCount =
  -- the engine stores max(1, min(materialCount, triangles)) material
  -- slots; a grid of x*z cells has 2 triangles per cell
  (`Mesh` max 1 (min materialCount (2 * xCount * zCount)))
    <$> wrapGenerated "gridMesh" B3Mesh.destroy (B3GridMesh.create xCount zCount cellWidth materialCount True)

{- | A torus mesh centered on the origin, its main ring lying in the local
XY plane (the tube's axis is local Z). 'radialResolution' is the number
of segments around the tube's circular cross-section and
'tubularResolution' is the number of segments around the main ring;
'radius' is the distance from the origin to the tube's center line and
'thickness' is the tube's cross-section radius.
-}
torusMesh :: Int -> Int -> Float -> Float -> IO Mesh
torusMesh radialResolution tubularResolution radius thickness =
  (`Mesh` 1) <$> wrapGenerated "torusMesh" B3Mesh.destroy (B3TorusMesh.create radialResolution tubularResolution radius thickness)

{- | A wavy grid mesh like 'gridMesh', with the vertex at row @ix@, column
@iz@ (0-based, along local x and z respectively) displaced to height
@amplitude * sin (2*pi*columnFrequency*cellWidth*ix) * sin
(2*pi*rowFrequency*cellWidth*iz)@. Note this is /not/ a typo in this
binding: in the upstream generator, 'columnFrequency' is the frequency
along x and 'rowFrequency' is the frequency along z — the reverse of
what the names suggest. Triangle adjacency is always identified, as
for 'gridMesh'.
-}
waveMesh :: Int -> Int -> Float -> Float -> Float -> Float -> IO Mesh
waveMesh xCount zCount cellWidth amplitude rowFrequency columnFrequency =
  (`Mesh` 1)
    <$> wrapGenerated
      "waveMesh"
      B3Mesh.destroy
      (B3WaveMesh.create xCount zCount cellWidth amplitude rowFrequency columnFrequency)

{- | A flat height-field grid of 'rowCount' * 'columnCount' samples.
'scale' converts grid index space to local space: index spacing along
x is @scale@\'s x component, spacing along z is its z component, and
sample heights (all zero for a flat grid) are multiplied by its y
component. Unlike the mesh generators, a height field's local origin
is a corner (grid index @(0, 0)@), not its center.

When 'makeHoles' is true, every 16th cell (by row-major index,
starting at the 16th) is punched out as a hole shapes fall through —
a fixed test pattern baked into the upstream generator, not a
configurable spacing; use 'gridMesh'\/a custom 'GeoMesh' instead if you
need holes somewhere specific.
-}
gridHeightField :: Int -> Int -> Vec3 -> Bool -> IO HeightField
gridHeightField rowCount columnCount scale makeHoles =
  HeightField <$> wrapGenerated "gridHeightField" B3HeightField.destroy (B3Grid.create rowCount columnCount scale makeHoles)

{- | A wavy height-field grid like 'gridHeightField', with the sample at
row @i@, column @j@ (0-based, in grid index space) set to
@sin (2*pi*rowFrequency*i) * sin (2*pi*columnFrequency*j)@ — unlike
'waveMesh', these frequencies are cycles per grid cell, not per local
unit. Raw samples are therefore always in @[-1, 1]@; scale the result
in local space with 'scale'\'s y component. 'makeHoles' is as in
'gridHeightField'.
-}
waveHeightField :: Int -> Int -> Vec3 -> Float -> Float -> Bool -> IO HeightField
waveHeightField rowCount columnCount scale rowFrequency columnFrequency makeHoles =
  HeightField
    <$> wrapGenerated
      "waveHeightField"
      B3HeightField.destroy
      (B3Wave.create rowCount columnCount scale rowFrequency columnFrequency makeHoles)

{- | A rock-shaped convex hull: 10 points spread over a sphere of the given
radius by a Fibonacci lattice, giving an irregular but bounded hull
useful for scatter/debris. Errors if the points come out degenerate
(e.g. 'radius' is zero, collapsing them to a point).
-}
rockHull :: Float -> IO Hull
rockHull radius =
  Hull <$> wrapGenerated "rockHull" B3Hull.destroy (B3Rock.create radius)

{- | A tessellated cone as a convex hull: a 'radius1' circle at local y 0
and a 'radius2' circle at local y 'height', joined by 'slices' sides
(the engine clamps this to [4, 32]). Equal radii give a cylinder-like
shape, but prefer 'cylinderHull' for that since it also supports an
axial offset.
-}
coneHull :: Float -> Float -> Float -> Int -> IO Hull
coneHull height radius1 radius2 slices =
  Hull <$> wrapGenerated "coneHull" B3Hull.destroy (B3Cone.create height radius1 radius2 slices)

{- | A tessellated cylinder as a convex hull: 'radius' circles at local y
'yOffset' and 'yOffset' + 'height', joined by 'sides' sides (the engine
clamps this to [3, 32]).
-}
cylinderHull :: Float -> Float -> Float -> Int -> IO Hull
cylinderHull height radius yOffset sides =
  Hull <$> wrapGenerated "cylinderHull" B3Hull.destroy (B3Cylinder.create height radius yOffset sides)
