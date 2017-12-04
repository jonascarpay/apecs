{-# LANGUAGE FlexibleContexts      #-}
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

makeWorld "World" [''Physics, ''BodyPicture, ''GlossView, ''Target]

initialize :: System World ()
initialize = do

  setGlobal ( GlossView 0 150
            , Gravity $ V2 0 (-10) )

  newEntity (StaticBody, Target)

  worldAnchor <- newEntity StaticBody

  let commonProps = (Friction 0.4, Elasticity 0.8, Density 1)

  let lineshape l = Segment (V2 (-l/2) 0, V2 (l/2) 0) 0.01
      line l = ( StaticBody
               , Shape $ lineshape l
               , BodyPicture . color white . fromShape $ lineshape l
               , commonProps )

  let ballshape = P.Circle 0 0.1
      ball = ( DynamicBody
             , Shape ballshape
             , BodyPicture . color red . fromShape $ ballshape
             , commonProps )

  let boxshape = Convex [V2 0 0, V2 0 0.2, V2 0.2 0.2, V2 0.2 0] 0
      box = ( DynamicBody
            , Shape boxshape
            , BodyPicture . color red . fromShape $ boxshape
            , commonProps )

  let paddleshape = Convex [V2 (-w) (-h), V2 (-w) h, V2 w h, V2 w (-h)] 0
      paddle pos = ( DynamicBody
                   , Position pos
                   , Shape paddleshape
                   , BodyPicture . color green . fromShape $ paddleshape
                   , Constraint worldAnchor (PivotJoint pos)
                   , commonProps )
      h = 0.2
      w = 0.03

  forM_ [-2 .. 2] $ \x ->
    newEntity (line 3, Position (V2 x 0), Angle (pi/2))

  forM_ [-1.5 .. 1.5] $ \y ->
    newEntity (line 4, Position (V2 0 y))

  springA <- newEntity (ball, Position (V2 (-1.7) 1))
  newEntity (ball, Position (V2 (-1.5) 1), Constraint (cast springA) $ DampedSpring 0 0 0.3 3 1e-4)

  pinB <- newEntity (box, Position (V2 (-0.3) 1))
  newEntity (box, Position (V2 (-0.55) 1), Constraint (cast pinB) (PinJoint (V2 0.2 0.2) (V2 0 0.2)))

  slideB <- newEntity (box, Position (V2 0.5 1))
  newEntity (box, Position (V2 0.75 1), Constraint (cast slideB) (SlideJoint (V2 0.2 0.2) (V2 0 0.2) 0 0.1))

  pivotA <- newEntity (box, Position (V2 1.1 1))
  pivotB <- newEntity (box, Position (V2 1.3 1), Constraint (cast pivotA) (PivotJoint (V2 1.3 1)))
  newEntity (box, Position (V2 1.5 1), Constraint (cast pivotB) (PivotJoint (V2 1.5 1)))

  drsA <- newEntity (paddle (V2 (-1.25) 0))
  drsB <- newEntity (paddle (V2 (-1.75) 0))
  newEntity (ConstraintExtend (cast drsA) (cast drsB) (GearJoint 0 3))

  drsA <- newEntity (paddle (V2 (-0.25) 0))
  drsB <- newEntity (paddle (V2 (-0.75) 0))
  newEntity (ConstraintExtend (cast drsA) (cast drsB) (DampedRotarySpring 0 1e-2 1e-4))

  rlA <- newEntity (paddle (V2 0.25 0))
  rlB <- newEntity (paddle (V2 0.75 0))
  newEntity (ConstraintExtend (cast rlA) (cast rlB) (RotaryLimitJoint 0 1))

  motA <- newEntity (paddle (V2 1.25 0))
  motB <- newEntity (paddle (V2 1.75 0))
  newEntity (ConstraintExtend (cast motA) (cast motB) (SimpleMotor pi))

  return ()

handle :: Event -> System World ()
handle (EventMotion mscreen) = do
  mpos <- mouseToWorld mscreen <$> getGlobal
  rmap $ \Target -> Position mpos

handle (EventKey (MouseButton LeftButton) Down _ mscreen) = do
  mpos <- mouseToWorld mscreen <$> getGlobal
  pq <- pointQuery mpos 0 defaultFilter
  case pq of
    Nothing                         -> return ()
    Just (PointQueryResult s _ _ _) -> rmap $
      \Target -> ( Constraint (cast s) (PivotJoint mpos), MaxForce 2, BodyPicture (color green $ G.Circle 0.03))

handle (EventKey (MouseButton LeftButton) Up _ _) =
  rmap' $ \Target -> Safe (Nothing,Nothing) :: Safe (Constraint, BodyPicture)

handle _ = return ()

main = do
  w <- initWorld
  runSystem initialize w
  playIO (InWindow "Constraint Gallery" (640,480) (100,100)) black 60 w render handler stepper
    where
      render w        = runSystem drawWorld w
      handler event w = runSystem (handle event) w >> return w
      stepper dT w    = runSystem (stepPhysics (realToFrac dT)) w >> return w

