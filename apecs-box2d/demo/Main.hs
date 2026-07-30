{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| A Box2D take on the apecs-physics tumbler example: a spinning
kinematic box full of bouncing balls, rendered with apecs-gloss.
Click anywhere to drop in more balls.
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Control.Monad (forM_, replicateM_)
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Linear (V2 (..))
import System.Random (randomRIO)

import Apecs.Box2D

-- | Local-space picture, placed by the entity's Box2D body transform.
newtype Look = Look Picture

instance Component Look where
  type Storage Look = Map Look

makeWorld "World" [''Physics, ''Look, ''Camera]

tumblerHalfSize, wallRadius :: Float
tumblerHalfSize = 2.5
wallRadius = 0.05

initialize :: SystemT World IO ()
initialize = do
  set global (Camera 0 70, Gravity (Vec2 0 (-10)))
  makeTumbler
  makeDumbbell
  replicateM_ 150 $ do
    x <- liftIO (randomRIO (-2, 2))
    y <- liftIO (randomRIO (-2, 2))
    spawnBall (Vec2 x y)

-- | Two green balls linked by a rigid distance joint.
makeDumbbell :: SystemT World IO ()
makeDumbbell = do
  let mk p = do
        e <- newEntity (DynamicBody, Position p, Look (Color chartreuse (circleSolid 0.15)))
        newEntity_ (Shape e (GeoCircle vec2Zero 0.15))
        pure e
  a <- mk (Vec2 (-0.8) 1.5)
  b <- mk (Vec2 0.8 1.5)
  newEntity_ (Joint a b (DistanceJoint (Vec2 (-0.8) 1.5) (Vec2 0.8 1.5)))

{- | A slowly rotating kinematic box whose walls are capsules laid
along the edges.
-}
makeTumbler :: SystemT World IO ()
makeTumbler = do
  let
    h = tumblerHalfSize
    corners = [(-h, -h), (h, -h), (h, h), (-h, h)]
    edges = zip corners (drop 1 corners ++ take 1 corners)
  tumbler <- newEntity (KinematicBody, AngularVelocity (-0.5), Look (Color white (lineLoop corners)))
  forM_ edges $ \((x1, y1), (x2, y2)) ->
    newEntity_ (Shape tumbler (GeoCapsule (Vec2 x1 y1) (Vec2 x2 y2) wallRadius))

spawnBall :: Vec2 -> SystemT World IO ()
spawnBall position = do
  r <- liftIO (randomRIO (0.08, 0.16))
  boxy <- liftIO (randomRIO (0 :: Int, 2))
  let
    tint = mixColors (0.16 - r) (r - 0.08) azure orange
    (geo, pic)
      | boxy == 0 = (GeoBox r r, rectangleSolid (2 * r) (2 * r))
      | otherwise = (GeoCircle vec2Zero r, circleSolid r)
  ball <- newEntity (DynamicBody, Position position, Look (Color tint pic))
  newEntity_ (Shape ball geo, Elasticity 0.2)

-- | Read each body's transform back from Box2D and place its picture.
draw :: SystemT World IO Picture
draw = foldDraw $ \(Position (Vec2 x y), Angle theta, Look pic) ->
  Translate x y (Rotate (negate (radToDeg theta)) pic)

handle :: Event -> SystemT World IO ()
handle (EventKey (MouseButton LeftButton) Down _ screenPos) = do
  camera <- get global
  let V2 x y = windowToWorld camera screenPos
  spawnBall (Vec2 x y)
handle _ = pure ()

step :: Float -> SystemT World IO ()
step dT = stepPhysics (min dT (1 / 30))

main :: IO ()
main = do
  w <- initWorld
  runSystem (initialize >> play disp black 60 draw handle step) w
  where
    disp = InWindow "apecs-box2d demo" (720, 720) (10, 10)
