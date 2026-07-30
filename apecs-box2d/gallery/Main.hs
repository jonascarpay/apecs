{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| A joint gallery, after the apecs-physics Constraints example: each
cell of the grid exhibits one 'JointSpec'. Top row, hanging from the
grid: a damped spring, a rigid rod pendulum, a slide, and a pivot
chain. Middle row: paddles on a rotary spring, a rotary limit and a
motor, plus a welded pair. Bottom row, joints composed: a seesaw, a
spring suspension, a trapeze and a spinner. Click to drop boxes on
things.
-}
module Main (main) where

import Apecs
import Apecs.Gloss
import Control.Monad (forM_)
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Linear (V2 (..))

import Apecs.Box2D

-- | Local-space picture, placed by the entity's Box2D body transform.
newtype Look = Look Picture

instance Component Look where
  type Storage Look = Map Look

makeWorld "World" [''Physics, ''Look, ''Camera]

gridSegs :: [((Float, Float), (Float, Float))]
gridSegs =
  [((x, -1.5), (x, 1.5)) | x <- [-2, -1, 0, 1, 2]]
    <> [((-2, y), (2, y)) | y <- [-1.5, -0.5, 0.5, 1.5]]

initialize :: SystemT World IO ()
initialize = do
  set global (Camera 0 150, earthGravity)

  grid <-
    newEntity
      ( StaticBody
      , Look (Color (greyN 0.4) (Pictures [Line [a, b] | (a, b) <- gridSegs]))
      )
  forM_ gridSegs $ \((x1, y1), (x2, y2)) ->
    newEntity_ (Shape grid (GeoSegment (Vec2 x1 y1) (Vec2 x2 y2)))

  let
    mkBall tint pos r = do
      e <- newEntity (DynamicBody, Position pos, Look (Color tint (circleSolid r)))
      newEntity_ (Shape e (GeoCircle vec2Zero r))
      pure e
    mkBox tint pos hw hh = do
      e <- newEntity (DynamicBody, Position pos, Look (Color tint (rectangleSolid (2 * hw) (2 * hh))))
      newEntity_ (Shape e (GeoBox hw hh))
      pure e

  -- top row: hanging from the grid line above
  spring <- mkBall azure (Vec2 (-1.5) 1.1) 0.1
  newEntity_ (Joint grid spring (SpringJoint (Vec2 (-1.5) 1.5) (Vec2 (-1.5) 1.1) 1 0.05))

  rod <- mkBall orange (Vec2 (-0.72) 1.2) 0.1
  newEntity_ (Joint grid rod (DistanceJoint (Vec2 (-0.5) 1.5) (Vec2 (-0.72) 1.2)))

  slide <- mkBall violet (Vec2 0.5 1.3) 0.1
  newEntity_ (Joint grid slide (SlideJoint (Vec2 0.5 1.5) (Vec2 0.5 1.3) 0.1 0.42))

  linkA <- mkBox rose (Vec2 1.5 1.4) 0.06 0.09
  linkB <- mkBox rose (Vec2 1.5 1.2) 0.06 0.09
  linkC <- mkBox rose (Vec2 1.5 1.0) 0.06 0.09
  newEntity_ (Joint grid linkA (PivotJoint (Vec2 1.5 1.5)))
  newEntity_ (Joint linkA linkB (PivotJoint (Vec2 1.5 1.3)))
  newEntity_ (Joint linkB linkC (PivotJoint (Vec2 1.5 1.1)))
  set linkC (Velocity (Vec2 1.5 0))

  -- middle row: paddles pinned to the grid, and a welded pair
  let mkPaddle tint cx spec = do
        paddle <- mkBox tint (Vec2 cx 0.12) 0.03 0.2
        newEntity_ (Joint paddle grid (spec (Vec2 cx 0)))
  mkPaddle aquamarine (-1.5) (\p -> RotarySpringJoint p 1 0.1)
  mkPaddle yellow (-0.5) (\p -> RotaryLimitJoint p (-0.6) 0.6)
  mkPaddle cyan 0.5 (\p -> RotaryMotorJoint p pi 5)

  weldA <- mkBox chartreuse (Vec2 1.4 0.1) 0.15 0.05
  weldB <- mkBox chartreuse (Vec2 1.5 (-0.05)) 0.05 0.15
  newEntity_ (Joint weldA weldB (WeldJoint (Vec2 1.45 0.02)))

  -- bottom row: composite exhibits
  -- a seesaw: a pivoted plank and a ball dropped on one end
  seesaw <- mkBox azure (Vec2 (-1.5) (-1.3)) 0.4 0.03
  newEntity_ (Joint seesaw grid (PivotJoint (Vec2 (-1.5) (-1.3))))
  _ <- mkBall white (Vec2 (-1.8) (-0.8)) 0.08

  -- suspension: a platform hanging on two springs, with a dropped ball
  platform <- mkBox orange (Vec2 (-0.5) (-1.2)) 0.25 0.04
  newEntity_ (Joint grid platform (SpringJoint (Vec2 (-0.7) (-0.5)) (Vec2 (-0.7) (-1.2)) 2 0.1))
  newEntity_ (Joint grid platform (SpringJoint (Vec2 (-0.3) (-0.5)) (Vec2 (-0.3) (-1.2)) 2 0.1))
  _ <- mkBall white (Vec2 (-0.5) (-0.7)) 0.08

  -- a trapeze: a plank on two rigid rods swings as a parallelogram
  trapeze <- mkBox rose (Vec2 0.5 (-1.1)) 0.2 0.03
  newEntity_ (Joint grid trapeze (DistanceJoint (Vec2 0.35 (-0.5)) (Vec2 0.35 (-1.1))))
  newEntity_ (Joint grid trapeze (DistanceJoint (Vec2 0.65 (-0.5)) (Vec2 0.65 (-1.1))))
  set trapeze (Velocity (Vec2 1.2 0))

  -- a spinner batting a couple of boxes around its cell
  spinner <- mkBox cyan (Vec2 1.5 (-1.15)) 0.3 0.05
  newEntity_ (Joint spinner grid (RotaryMotorJoint (Vec2 1.5 (-1.15)) 2 10))
  _ <- mkBox white (Vec2 1.3 (-0.7)) 0.07 0.07
  _ <- mkBox white (Vec2 1.7 (-0.7)) 0.07 0.07

  cmap $ \(_ :: Shape) -> (Friction 0.4, Elasticity 0.8, Density 1)

{- | Lines for the distance-flavored joints, from live body positions
(static ends use the creation-time anchor).
-}
drawJoints :: SystemT World IO Picture
drawJoints = cfoldM link mempty
  where
    link pic (Joint ea eb spec) = case anchors spec of
      Nothing -> pure pic
      Just (aA, aB) -> do
        Vec2 x1 y1 <- endpoint ea aA
        Vec2 x2 y2 <- endpoint eb aB
        pure (pic <> Color (greyN 0.6) (Line [(x1, y1), (x2, y2)]))
    anchors (SpringJoint a b _ _) = Just (a, b)
    anchors (DistanceJoint a b) = Just (a, b)
    anchors (SlideJoint a b _ _) = Just (a, b)
    anchors _ = Nothing
    endpoint e anchor = do
      body <- get e
      case body of
        StaticBody -> pure anchor
        _ -> do
          Position p <- get e
          pure p

draw :: SystemT World IO Picture
draw = do
  joints <- drawJoints
  bodies <- foldDraw $ \(Position (Vec2 x y), Angle theta, Look pic) ->
    Translate x y (Rotate (negate (radToDeg theta)) pic)
  pure (joints <> bodies)

{- | Keep spawn points clear of the grid lines: a box born overlapping a
zero-thickness segment has no escape direction and settles wedged.
-}
awayFromLines :: V2 Float -> Vec2
awayFromLines (V2 x y) = Vec2 (away vLine x) (away hLine y)
  where
    vLine = fromIntegral (round x :: Int)
    hLine = fromIntegral (round (y - 0.5) :: Int) + 0.5
    away l v
      | abs (v - l) < 0.18 = l + (if v >= l then 0.18 else -0.18)
      | otherwise = v

handle :: Event -> SystemT World IO ()
handle (EventKey (MouseButton LeftButton) Down _ screenPos) = do
  camera <- get global
  box <-
    newEntity
      ( DynamicBody
      , Position (awayFromLines (windowToWorld camera screenPos))
      , Look (Color white (rectangleSolid 0.24 0.24))
      )
  newEntity_ (Shape box (GeoBox 0.12 0.12), Friction 0.4, Elasticity 0.5)
handle _ = pure ()

step :: Float -> SystemT World IO ()
step dT = stepPhysics (min dT (1 / 30))

main :: IO ()
main = do
  w <- initWorld
  runSystem (initialize >> play disp black 60 draw handle step) w
  where
    disp = InWindow "apecs-box2d joint gallery" (640, 640) (10, 10)
