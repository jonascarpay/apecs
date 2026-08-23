{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The tumbler, one dimension up: a spinning kinematic box full of
spheres and cubes, simulated by Box3D and rendered with apecs-gloss-3d
(painter's-algorithm CPU projection — see Apecs.Gloss3D for how that
works without a depth buffer). Click to drop in more debris. The
camera orbits on its own.
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Apecs.Gloss3D
import Control.Monad (forM_, replicateM_)
import Linear (Quaternion (..), V3 (..))
import System.Random (randomRIO)

import Apecs.Box3D

-- | How an entity is rendered; physics state comes from its body.
data Look
  = LookBall Color Float
  | LookCube Color Vec3
  | LookCage Float

instance Component Look where
  type Storage Look = Map Look

-- | Camera orbit angle, advanced every frame.
newtype Orbit = Orbit Float

instance Semigroup Orbit where
  _ <> b = b

instance Monoid Orbit where
  mempty = Orbit 0

instance Component Orbit where
  type Storage Orbit = Global Orbit

makeWorld "World" [''Physics, ''Look, ''Orbit, ''Env3, ''Camera]

-- | Box3D vectors to linear, at the rendering boundary.
v3 :: Vec3 -> V3 Float
v3 (Vec3 x y z) = V3 x y z

-- | Box3D quaternions to linear (which carries w first).
q3 :: Quat -> Quaternion Float
q3 (Quat (Vec3 x y z) w) = Quaternion w (V3 x y z)

cageHalf, wallThickness :: Float
cageHalf = 2.5
wallThickness = 0.15

initialize :: SystemT World IO ()
initialize = do
  set global (Gravity (Vec3 0 (-10) 0))
  makeCage
  makeDumbbell
  replicateM_ 70 (spawnDebris False)
  replicateM_ 25 (spawnDebris True)

{- | A kinematic box cage tumbling about two axes. The walls are thin
boxes just outside the cavity; only the cavity edges are drawn, as a
wireframe.
-}
makeCage :: SystemT World IO ()
makeCage = do
  cage <-
    newEntity
      ( KinematicBody
      , AngularVelocity (Vec3 0.15 0 (-0.4))
      , LookCage cageHalf
      )
  let
    h = cageHalf
    t = wallThickness
    walls =
      [ (Vec3 (s * (h + t)) 0 0, Vec3 t (h + 2 * t) (h + 2 * t))
      | s <- [-1, 1]
      ]
        <> [ (Vec3 0 (s * (h + t)) 0, Vec3 (h + 2 * t) t (h + 2 * t))
           | s <- [-1, 1]
           ]
        <> [ (Vec3 0 0 (s * (h + t)), Vec3 (h + 2 * t) (h + 2 * t) t)
           | s <- [-1, 1]
           ]
  forM_ walls $ \(center, half) ->
    newEntity_ (Shape cage (GeoBox center half))

-- | Two green spheres linked by a rigid distance joint.
makeDumbbell :: SystemT World IO ()
makeDumbbell = do
  let mk p = do
        e <- newEntity (DynamicBody, Position p, LookBall chartreuse 0.22)
        newEntity_ (Shape e (GeoSphere vec3Zero 0.22))
        pure e
  a <- mk (Vec3 (-0.7) 1.5 0)
  b <- mk (Vec3 0.7 1.5 0)
  newEntity_ (Joint a b (DistanceJoint (Vec3 (-0.7) 1.5 0) (Vec3 0.7 1.5 0)))

spawnDebris :: Bool -> SystemT World IO ()
spawnDebris cubic = do
  let range = cageHalf - 0.7
  x <- liftIO (randomRIO (-range, range))
  y <- liftIO (randomRIO (-range, range))
  z <- liftIO (randomRIO (-range, range))
  r <- liftIO (randomRIO (0.15, 0.3))
  let
    tint = mixColors (0.3 - r) (r - 0.15) azure orange
    look
      | cubic = LookCube tint (Vec3 r r r)
      | otherwise = LookBall tint r
  body <- newEntity (DynamicBody, Position (Vec3 x y z), look)
  let geo
        | cubic = GeoBox vec3Zero (Vec3 r r r)
        | otherwise = GeoSphere vec3Zero r
  newEntity_ (Shape body geo, Elasticity 0.25)

drawLook :: Scene3 -> Vec3 -> Quat -> Look -> [Piece]
drawLook sc p q look = case look of
  LookBall tint r -> sphere3 sc tint r (v3 p)
  LookCube tint half -> solid3 sc tint (boxSolid (v3 half)) (v3 p) (q3 q)
  LookCage h -> wire3 sc (greyN 0.85) (boxEdges (V3 h h h)) (v3 p) (q3 q)

draw :: SystemT World IO Picture
draw = do
  Orbit a <- get global
  env <- get global
  let sc = scene3 (orbitCamera a 9 4 520) env
  foldDraw3 $ \(Position p, Rotation q, look :: Look) -> drawLook sc p q look

handle :: Event -> SystemT World IO ()
handle (EventKey (MouseButton LeftButton) Down _ _) = do
  cubic <- liftIO (randomRIO (0 :: Int, 2))
  spawnDebris (cubic == 0)
handle _ = pure ()

step :: Float -> SystemT World IO ()
step dT = do
  stepPhysics (min dT (1 / 30))
  Orbit a <- get global
  set global (Orbit (a + dT * 0.25))

main :: IO ()
main = do
  w <- initWorld
  runSystem (initialize >> play disp black 60 draw handle step) w
  where
    disp = InWindow "apecs-box3d tumbler" (720, 720) (10, 10)
