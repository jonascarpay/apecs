module Apecs.Physics.Geometry where

import Apecs.Physics.Types
import Linear

vertices :: Convex -> [BVec]
vertices (Convex s _) = s

-- | Map a function over all vertices
mapVertices :: (BVec -> BVec) -> Convex -> Convex
mapVertices f (Convex s r) = Convex (f <$> s) r

-- | Translates all vertices. The name shift is to prevent collisions with gloss
shift :: BVec -> Convex -> Convex
shift = mapVertices . (+)

getRadius :: Convex -> Double
getRadius (Convex _ r) = r

setRadius :: Double -> Convex -> Convex
setRadius r (Convex s _) = Convex s r

cCircle, zCircle :: Double -> Convex
cCircle r = oCircle 0 r
zCircle = cCircle

oCircle :: BVec -> Double -> Convex
oCircle o r = Convex [o] r

hLine, vLine :: Double -> Convex
hLine l = Convex [V2 (-l/2) 0, V2 (l/2) 0] 0
vLine l = Convex [V2 0 (l/2), V2 0 (-l/2)] 0

-- | Centered rectangle with a given size
cRectangle :: BVec -> Convex
cRectangle s = oRectangle (-s*0.5) s

-- | Rectangle with a given origin and size
oRectangle :: BVec -> BVec -> Convex
oRectangle (V2 x y) (V2 w h) = Convex [V2 x y, V2 x (y+h), V2 (x+w) (y+h), V2 (x+w) y] 0

-- | Rectangle with origin 0 and given size
zRectangle :: BVec -> Convex
zRectangle s = oRectangle 0 s

-- | Split a shape into its edges. Will return no edges for points, but returns 2 for a line (in opposite directions)
toEdges :: Convex -> [Convex]
toEdges (Convex [] _) = []
toEdges (Convex [_] _) = []
toEdges (Convex vs r) = zipWith (\h t -> Convex [h,t] r) vs (tail . cycle $ vs)

-- | A set of lines forming a grid. Returns (r + c + 2) segments.
gridLines :: Vec -> Int -> Int -> [Convex]
gridLines size c r =
  [ shift (V2 x 0) (vLine h) | x <- xs ] ++
  [ shift (V2 0 y) (hLine w) | y <- ys ]
  where
    V2 w h = size
    V2 x y = -size*0.5
    dx = w/fromIntegral c
    dy = h/fromIntegral r
    xs = [x + fromIntegral n * dx | n <- [0..c]]
    ys = [y + fromIntegral n * dy | n <- [0..r]]

