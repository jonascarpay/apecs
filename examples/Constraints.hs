{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics                    as P
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad

data Target = Target
instance Component Target
  where type Storage Target = Unique Target

makeWorld "World" [''Physics, ''Camera, ''Target]

material = (Friction 0.4, Elasticity 0.8, Density 1)

initialize = do
  set global ( Camera 0 150
             , earthGravity )

  let gridLines' = gridLines (V2 4 3) 4 3

  grid <- newEntity ( StaticBody
                    , material
                    )
  forM_ gridLines' $ \line ->
    newEntity ( ShapeExtend grid (setRadius 0.01 line), material )

  let ball = ( DynamicBody
             , Shape (cCircle 0.1)
             , material )

  springA <- newEntity (ball, Position (V2 (-1.7) 1))
  newEntity (ball, Position (V2 (-1.5) 1), Constraint springA $ DampedSpring 0 0 0.3 3 1e-4)

  let box = ( DynamicBody
            , Shape (oRectangle 0 0.2)
            , material )

  pinB <- newEntity (box, Position (V2 (-0.3) 1))
  newEntity (box, Position (V2 (-0.55) 1), Constraint pinB (PinJoint (V2 0.2 0.2) (V2 0 0.2)))

  slideB <- newEntity (box, Position (V2 0.5 1))
  newEntity (box, Position (V2 0.75 1), Constraint slideB (SlideJoint (V2 0.2 0.2) (V2 0 0.2) 0 0.1))

  pivotA <- newEntity (box, Position (V2 1.1 1))
  pivotB <- newEntity (box, Position (V2 1.3 1), Constraint pivotA (PivotJoint (V2 1.3 1)))
  newEntity (box, Position (V2 1.5 1), Constraint pivotB (PivotJoint (V2 1.5 1)))

  let paddle pos = ( DynamicBody
                   , Position pos
                   , Shape (cRectangle (V2 0.06 0.4))
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
  mpos <- flip windowToWorld mscreen <$> get global
  cmap $ \Target -> Position mpos

handle (EventKey (MouseButton LeftButton) Down _ mscreen) = do
  mpos <- flip windowToWorld mscreen <$> get global
  pq <- pointQuery mpos 0 defaultFilter
  case pq of
    Nothing                         -> return ()
    Just (PointQueryResult s _ _ _) -> cmap $
      \Target -> ( Constraint s (PivotJoint mpos), MaxForce 2)

handle (EventKey (MouseButton LeftButton) Up _ _) =
  cmap $ \Target -> Not :: Not Constraint

handle (EventKey (MouseButton RightButton) Down _ mscreen) = do
  mpos <- flip windowToWorld mscreen <$> get global
  newEntity ( DynamicBody
            , Position mpos
            , Shape (cRectangle 0.3)
            , material )
  return ()

handle _ = return ()

disp = InWindow "Constraint Gallery" (640,640) (10,10)
main = (initWorld >>=) . runSystem $ do
  initialize
  play disp
       black
       60
       (foldDrawM drawBody)
       handle
       (const $ stepPhysics (1/60))
