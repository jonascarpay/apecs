{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics                    as P
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad
import           Graphics.Gloss                   as G
import           Graphics.Gloss.Interface.IO.Game as G

data Target = Target
instance Component Target
  where type Storage Target = Unique Target

makeWorld "World" [''Physics, ''BodyPicture, ''Camera, ''Target]

material = (Friction 0.4, Elasticity 0.8, Density 1)

initialize = do
  set global ( Camera 0 150
             , earthGravity )

  let gridLines' = gridLines (V2 4 3) 4 3

  grid <- newEntity ( StaticBody
                    , material
                    , BodyPicture . color white . foldMap toPicture $ gridLines' )
  forM_ gridLines' $ \line ->
    newEntity ( ShapeExtend grid (setRadius 0.01 line), material )

  let ballshape = cCircle 0.1
      ball = ( DynamicBody
             , Shape ballshape
             , BodyPicture . color red . toPicture $ ballshape
             , material )

  springA <- newEntity (ball, Position (V2 (-1.7) 1))
  newEntity (ball, Position (V2 (-1.5) 1), Constraint springA $ DampedSpring 0 0 0.3 3 1e-4)

  let boxshape = oRectangle 0 0.2
      box = ( DynamicBody
            , Shape boxshape
            , BodyPicture . color red . toPicture $ boxshape
            , material )

  pinB <- newEntity (box, Position (V2 (-0.3) 1))
  newEntity (box, Position (V2 (-0.55) 1), Constraint pinB (PinJoint (V2 0.2 0.2) (V2 0 0.2)))

  slideB <- newEntity (box, Position (V2 0.5 1))
  newEntity (box, Position (V2 0.75 1), Constraint slideB (SlideJoint (V2 0.2 0.2) (V2 0 0.2) 0 0.1))

  pivotA <- newEntity (box, Position (V2 1.1 1))
  pivotB <- newEntity (box, Position (V2 1.3 1), Constraint pivotA (PivotJoint (V2 1.3 1)))
  newEntity (box, Position (V2 1.5 1), Constraint pivotB (PivotJoint (V2 1.5 1)))

  let paddleshape = cRectangle (V2 0.06 0.4)
      paddle pos = ( DynamicBody
                   , Position pos
                   , Shape paddleshape
                   , BodyPicture . color green . toPicture $ paddleshape
                   , Constraint grid (PivotJoint pos)
                   , material )

  drsA <- newEntity (paddle (V2 (-1.25) 0))
  drsB <- newEntity (paddle (V2 (-1.75) 0))
  newEntity (ConstraintExtend drsA drsB (GearJoint 0 3))

  drsA <- newEntity (paddle (V2 (-0.25) 0))
  drsB <- newEntity (paddle (V2 (-0.75) 0))
  newEntity (ConstraintExtend drsA drsB (DampedRotarySpring 0 1e-2 1e-4))

  rlA <- newEntity (paddle (V2 0.25 0))
  rlB <- newEntity (paddle (V2 0.75 0))
  newEntity (ConstraintExtend rlA rlB (RotaryLimitJoint 0 1))

  motA <- newEntity (paddle (V2 1.25 0))
  motB <- newEntity (paddle (V2 1.75 0))
  newEntity (ConstraintExtend motA motB (SimpleMotor pi))

  newEntity (StaticBody, Target)

handle :: Event -> System World ()
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  cmap $ \Target -> Position mpos

handle (EventKey (MouseButton LeftButton) Down _ mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  pq <- pointQuery mpos 0 defaultFilter
  case pq of
    Nothing                         -> return ()
    Just (PointQueryResult s _ _ _) -> cmap $
      \Target -> ( Constraint s (PivotJoint mpos), MaxForce 2, BodyPicture (color green $ G.Circle 0.03))

handle (EventKey (MouseButton LeftButton) Up _ _) =
  cmap $ \Target -> Not :: Not (Constraint, BodyPicture)

handle (EventKey (MouseButton RightButton) Down _ mscreen) = do
  mpos <- mouseToWorld mscreen <$> get global
  let sh = cRectangle 0.3
  newEntity ( DynamicBody
            , Position mpos
            , Shape sh
            , BodyPicture . color blue . toPicture $ sh
            , material )
  return ()

handle _ = return ()

main = do
  w <- initWorld
  runSystem initialize w
  playIO (InWindow "Constraint Gallery" (640,480) (100,100)) black 60 w render handler stepper
    where
      render w        = runSystem drawWorld w
      handler event w = runSystem (handle event) w >> return w
      stepper dT w    = runSystem (stepPhysics (1/60)) w >> return w

