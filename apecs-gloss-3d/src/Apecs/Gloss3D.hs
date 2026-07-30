{-# LANGUAGE TypeFamilies #-}

{-| Depth-sorted 3D rendering on top of gloss, for prototyping — the 3D
sibling of apecs-gloss, with no physics-engine dependency.

Gloss has no depth buffer, but two facts make one unnecessary for
scenes of solid bodies:

1. Disjoint bodies sorted by view depth (painter's algorithm) produce
   the correct occlusion order.
2. The front faces of a /convex/ polyhedron never overlap each other in
   projection, so per-body rendering needs only backface culling.

Primitives return depth-tagged 'Piece's; 'assemble' sorts them
far-to-near into a single 'Picture'. The known failure mode is large
flat geometry crossing other geometry (intersecting planes, ramps seen
edge-on) — keep the primitive set convex and disjoint.

Everything is per-frame CPU work: a few thousand pieces at 60fps is
comfortable; tens of thousands is not the target.
-}
module Apecs.Gloss3D
  ( -- * Camera
    Camera3 (..)
  , orbitCamera

    -- * Environment
  , Env3 (..)
  , env3

    -- * Scene
  , Scene3
  , scene3
  , viewPoint
  , projectPoint

    -- * Pieces
  , Piece
  , assemble
  , foldDraw3

    -- * Solids
  , Solid (..)
  , facesSolid
  , boxSolid
  , cubeSolid
  , boxEdges
  , solidEdges

    -- * Primitives
  , sphere3
  , solid3
  , hiddenWire3
  , wire3
  , blob3
  ) where

import Apecs
import Data.List (nub, sortOn)
import Graphics.Gloss hiding (rotate, scale)
import Linear

{- | A look-at perspective camera. 'c3Focal' is the projection scale:
pixels per world unit at depth 1.
-}
data Camera3 = Camera3
  { c3Eye :: V3 Float
  , c3Target :: V3 Float
  , c3Up :: V3 Float
  , c3Focal :: Float
  }
  deriving (Eq, Show)

-- | Later camera wins.
instance Semigroup Camera3 where
  _ <> b = b

instance Monoid Camera3 where
  mempty = Camera3 (V3 0 2 10) (V3 0 0 0) (V3 0 1 0) 400

instance Component Camera3 where
  type Storage Camera3 = Global Camera3

{- | A camera circling the origin: angle around the Y axis, distance,
height, focal length.
-}
orbitCamera :: Float -> Float -> Float -> Float -> Camera3
orbitCamera theta dist height focal =
  Camera3
    { c3Eye = V3 (dist * cos theta) height (dist * sin theta)
    , c3Target = V3 0 0 0
    , c3Up = V3 0 1 0
    , c3Focal = focal
    }

{- | Lighting and depth cues: a directional light with an ambient floor,
and distance haze dimming colors from 'e3FogNear' (full brightness)
to 'e3FogFar' (dimmed to 'e3FogFloor').
-}
data Env3 = Env3
  { e3Light :: V3 Float
  , e3Ambient :: Float
  , e3FogNear :: Float
  , e3FogFar :: Float
  , e3FogFloor :: Float
  }
  deriving (Eq, Show)

-- | Later environment wins.
instance Semigroup Env3 where
  _ <> b = b

instance Monoid Env3 where
  mempty = env3

instance Component Env3 where
  type Storage Env3 = Global Env3

-- | A serviceable default environment.
env3 :: Env3
env3 =
  Env3
    { e3Light = normalize (V3 (-0.4) 1 0.55)
    , e3Ambient = 0.35
    , e3FogNear = 5
    , e3FogFar = 18
    , e3FogFloor = 0.35
    }

{- | A camera and environment resolved for one frame: the view basis is
precomputed, so build it once per frame with 'scene3'.
-}
data Scene3 = Scene3
  { scEye :: !(V3 Float)
  , scRight :: !(V3 Float)
  , scUp :: !(V3 Float)
  , scFwd :: !(V3 Float)
  , scFocal :: !Float
  , scEnv :: !Env3
  }

{- | Resolve a camera and environment for the frame. Degenerate when the
view direction is parallel to the camera's up vector.
-}
scene3 :: Camera3 -> Env3 -> Scene3
scene3 cam env =
  Scene3
    { scEye = c3Eye cam
    , scRight = right
    , scUp = up
    , scFwd = fwd
    , scFocal = c3Focal cam
    , scEnv = env
    }
  where
    fwd = normalize (c3Target cam - c3Eye cam)
    right = normalize (cross fwd (c3Up cam))
    up = cross right fwd

-- | To view space: x right, y up, z depth into the screen.
viewPoint :: Scene3 -> V3 Float -> V3 Float
viewPoint sc p = V3 (dot d (scRight sc)) (dot d (scUp sc)) (dot d (scFwd sc))
  where
    d = p - scEye sc

-- | Perspective-divide a view-space point to screen coordinates.
projectPoint :: Scene3 -> V3 Float -> (Float, Float)
projectPoint sc (V3 x y z) = (scFocal sc * x / z, scFocal sc * y / z)

-- | Geometry closer than this view depth is culled.
nearPlane :: Float
nearPlane = 0.2

{- | Distance-haze brightness for a view depth. The denominator is
clamped so a degenerate span (fogNear == fogFar) gives a hard step at
that depth instead of NaN colors.
-}
fogK :: Scene3 -> Float -> Float
fogK sc z = max (e3FogFloor env) (min 1 (1 - k * (1 - e3FogFloor env)))
  where
    env = scEnv sc
    k = (z - e3FogNear env) / max 1e-6 (e3FogFar env - e3FogNear env)

-- | Scale a color's brightness.
shade :: Float -> Color -> Color
shade k c = mixColors k (1 - k) c black

-- | Lambert term against the scene light with the ambient floor.
lambert :: Scene3 -> V3 Float -> Float
lambert sc n = a + (1 - a) * max 0 (dot n (e3Light (scEnv sc)))
  where
    a = e3Ambient (scEnv sc)

{- | A depth-tagged picture fragment; the depth is the view-space
distance used by the painter's sort.
-}
type Piece = (Float, Picture)

-- | Paint far-to-near.
assemble :: [Piece] -> Picture
assemble = Pictures . map snd . sortOn (negate . fst)

{- | Fold a component's pieces over all entities and 'assemble' them;
the 3D analog of apecs-gloss's @foldDraw@.
-}
foldDraw3
  :: (Get w IO c, Members w IO c)
  => (c -> [Piece])
  -> SystemT w IO Picture
foldDraw3 fn = assemble <$> cfold (\pieces c -> fn c <> pieces) []

{- | A convex polyhedron as faces: outward unit normal and corners in
cycle order, in body-local space. Correct rendering relies on
convexity.
-}
newtype Solid = Solid [(V3 Float, [V3 Float])]

{- | A solid from faces given as corner cycles in body-local space.
Outward unit normals are computed from each face's winding (Newell's
method) and flipped to point away from the solid's centroid, so the
winding direction need not be consistent across faces. Faces should
be planar and the solid convex; degenerate faces (fewer than three
corners, or collinear/coincident ones with no area) are dropped.
-}
facesSolid :: [[V3 Float]] -> Solid
facesSolid faces = Solid [face | cs <- faces, length cs >= 3, face <- orient cs]
  where
    corners = concat faces
    centroid = sum corners ^/ fromIntegral (length corners)
    orient cs
      -- a zero Newell normal has no direction to normalize; keeping it
      -- would NaN-poison the lighting and culling dot products
      | quadrance nw > 1e-12 = [(if dot n (c - centroid) < 0 then negate n else n, cs)]
      | otherwise = []
      where
        nw = newell cs
        n = normalize nw
        c = sum cs ^/ fromIntegral (length cs)
    newell cs = sum [cross a b | (a, b) <- zip cs (drop 1 cs <> take 1 cs)]

-- | An axis-aligned box from half-extents.
boxSolid :: V3 Float -> Solid
boxSolid half =
  Solid
    [ (n, [c * half | c <- cs])
    | (n, cs) <- unitFaces
    ]
  where
    unitFaces =
      [ (V3 1 0 0, [V3 1 1 1, V3 1 1 (-1), V3 1 (-1) (-1), V3 1 (-1) 1])
      , (V3 (-1) 0 0, [V3 (-1) 1 1, V3 (-1) 1 (-1), V3 (-1) (-1) (-1), V3 (-1) (-1) 1])
      , (V3 0 1 0, [V3 1 1 1, V3 1 1 (-1), V3 (-1) 1 (-1), V3 (-1) 1 1])
      , (V3 0 (-1) 0, [V3 1 (-1) 1, V3 1 (-1) (-1), V3 (-1) (-1) (-1), V3 (-1) (-1) 1])
      , (V3 0 0 1, [V3 1 1 1, V3 1 (-1) 1, V3 (-1) (-1) 1, V3 (-1) 1 1])
      , (V3 0 0 (-1), [V3 1 1 (-1), V3 1 (-1) (-1), V3 (-1) (-1) (-1), V3 (-1) 1 (-1)])
      ]

-- | A cube from a half-width.
cubeSolid :: Float -> Solid
cubeSolid h = boxSolid (V3 h h h)

{- | The unique edges of a 'Solid', for 'wire3': segments shared by two
faces (or bounding a lone face) appear once.
-}
solidEdges :: Solid -> [(V3 Float, V3 Float)]
solidEdges (Solid faces) = nub [ordered a b | (_, cs) <- faces, (a, b) <- zip cs (drop 1 cs <> take 1 cs)]
  where
    ordered a b = if a <= b then (a, b) else (b, a)

-- | The 12 edges of an axis-aligned box, for 'wire3'.
boxEdges :: V3 Float -> [(V3 Float, V3 Float)]
boxEdges = solidEdges . boxSolid

{- | A sphere impostor: a projected disc with an offset highlight,
lit by how squarely the surface patch facing the camera meets the
light.
-}
sphere3 :: Scene3 -> Color -> Float -> V3 Float -> [Piece]
sphere3 sc col r pos =
  -- depth-keyed by the nearest point, not the center, so a big near
  -- sphere sorts in front of the small far pieces it occludes
  [ (z - r, Translate px py (Pictures [body, glint]))
  | let v@(V3 _ _ z) = viewPoint sc pos
  , z > nearPlane
  , let
      (px, py) = projectPoint sc v
      rs = scFocal sc * r / z
      base = shade (lambert sc (normalize (scEye sc - pos)) * fogK sc z) col
      body = Color base (circleSolid rs)
      glint = Color (mixColors 0.6 0.4 base white) (Translate (-rs * 0.3) (rs * 0.3) (circleSolid (rs * 0.35)))
  ]

{- | The visible faces of a 'Solid' with the given transform, one piece
per front face.
-}
solidFaces :: Scene3 -> Solid -> V3 Float -> Quaternion Float -> [(Float, Float, [(Float, Float)])]
solidFaces sc (Solid faces) pos q =
  [ (depth, lit, pts)
  | (ln, cs) <- faces
  , let
      n = rotate q ln
      ws = [pos + rotate q c | c <- cs]
      count = fromIntegral (length ws)
      center = sum ws ^/ count
  , dot n (center - scEye sc) < 0
  , let vs = map (viewPoint sc) ws
  , all (\(V3 _ _ z) -> z > nearPlane) vs
  , let
      depth = sum [z | V3 _ _ z <- vs] / count
      lit = lambert sc n
      pts = map (projectPoint sc) vs
  ]

{- | A flat-shaded convex solid: backface-culled faces joining the
global paint order individually.
-}
solid3 :: Scene3 -> Color -> Solid -> V3 Float -> Quaternion Float -> [Piece]
solid3 sc col solid pos q =
  [ (depth, Color (shade (lit * fogK sc depth) col) (Polygon pts))
  | (depth, lit, pts) <- solidFaces sc solid pos q
  ]

{- | Old-school hidden-line wireframe: front faces filled with the
background color and stroked with the edge color. Backface culling
plus the painter's order gives hidden-line removal for free.
-}
hiddenWire3 :: Scene3 -> Color -> Color -> Solid -> V3 Float -> Quaternion Float -> [Piece]
hiddenWire3 sc edgeCol fillCol solid pos q =
  [ (depth, Pictures [Color fillCol (Polygon pts), Color (shade (fogK sc depth) edgeCol) (lineLoop pts)])
  | (depth, _, pts) <- solidFaces sc solid pos q
  ]

{- | Body-local line segments with the given transform, subdivided so
long edges sort correctly against bodies passing in front of and
behind them.
-}
wire3 :: Scene3 -> Color -> [(V3 Float, V3 Float)] -> V3 Float -> Quaternion Float -> [Piece]
wire3 sc col segs pos q =
  [ (zm, Color (shade (fogK sc zm) col) (Line [projectPoint sc a, projectPoint sc b]))
  | (c1, c2) <- segs
  , let
      w1 = pos + rotate q c1
      w2 = pos + rotate q c2
      vs = [viewPoint sc (lerp (fromIntegral i / n) w1 w2) | i <- [0 .. subdivs]]
  , (a, b) <- zip vs (drop 1 vs)
  , let zm = (vz a + vz b) / 2
  , vz a > nearPlane && vz b > nearPlane
  ]
  where
    subdivs = 6 :: Int
    n = fromIntegral subdivs
    vz (V3 _ _ z) = z

{- | A soft shadow blob: a flattened dark ellipse at a ground point,
drawn just behind anything standing on it. The cheapest strong depth
cue there is.
-}
blob3 :: Scene3 -> Color -> Float -> V3 Float -> [Piece]
blob3 sc col r pos =
  [ (z + r, Translate px py (Color col (Scale 1 0.35 (circleSolid rs))))
  | let v@(V3 _ _ z) = viewPoint sc pos
  , z > nearPlane
  , let
      (px, py) = projectPoint sc v
      rs = scFocal sc * r / z
  ]
