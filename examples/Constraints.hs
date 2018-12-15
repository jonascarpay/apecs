{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Apecs.Physics       as P
import           Apecs.Physics.Gloss
import           Apecs.Util
import           Control.Monad

data MouseBody = MouseBody
instance Component MouseBody
  where type Storage MouseBody = Unique MouseBody

makeWorld "World" [''Physics, ''Camera, ''MouseBody]

material = (Friction 0.4, Elasticity 0.8, Density 1)

collisionFilter = CollisionFilter 1 maskAll maskAll

initialize = do
  set global ( Camera 0 150
             , earthGravity )

  let gridLines' = gridLines (V2 4 3) 4 3

  grid <- newEntity StaticBody
  forM_ gridLines' $ \gline ->
    newEntity (Shape grid $ setRadius 0.01 gline, collisionFilter)

  let mkBall pos = do
        ball <- newEntity (DynamicBody, pos)
        newEntity (Shape ball (cCircle 0.1))
        return ball

  springA <- mkBall $ Position (V2 (-1.5) 1)
  springB <- mkBall $ Position (V2 (-1.7) 1)
  newEntity $ Constraint springA springB (DampedSpring 0 0 0.3 3 1e-4)

  let mkBox pos = do
        box <- newEntity (DynamicBody, pos)
        newEntity (Shape box (oRectangle 0 0.2))
        return box

  pinA <- mkBox $ Position (V2 (-0.55) 1)
  pinB <- mkBox $ Position (V2 (-0.3) 1)
  newEntity $ Constraint pinA pinB (PinJoint (V2 0.2 0.2) (V2 0 0.2))

  slideA <- mkBox $ Position (V2 0.75 1)
  slideB <- mkBox $ Position (V2 0.5 1)
  newEntity $ Constraint slideA slideB (SlideJoint (V2 0.2 0.2) (V2 0 0.2) 0 0.1)

  pivotA <- mkBox $ Position (V2 1.1 1)
  pivotB <- mkBox $ Position (V2 1.3 1)
  pivotC <- mkBox $ Position (V2 1.5 1)
  newEntity $ Constraint pivotA pivotB (PivotJoint (V2 1.3 1))
  newEntity $ Constraint pivotB pivotC (PivotJoint (V2 1.5 1))

  let mkPaddle (Position pos) = do
        paddle <- newEntity (DynamicBody, Position pos)
        newEntity $ Shape paddle (cRectangle (V2 0.06 0.4))
        newEntity $ Constraint paddle grid (PivotJoint pos)
        return paddle

  gearA <- mkPaddle $ Position (V2 (-1.25) 0)
  gearB <- mkPaddle $ Position (V2 (-1.75) 0)
  newEntity $ Constraint gearA gearB (GearJoint 0 3)

  drsA <- mkPaddle $ Position (V2 (-0.25) 0)
  drsB <- mkPaddle $ Position (V2 (-0.75) 0)
  newEntity $ Constraint drsA drsB (DampedRotarySpring 0 1e-2 1e-4)

  rlA <- mkPaddle $ Position (V2 0.25 0)
  rlB <- mkPaddle $ Position (V2 0.75 0)
  newEntity (Constraint rlA rlB (RotaryLimitJoint 0 1))

  motA <- mkPaddle $ Position (V2 1.25 0)
  motB <- mkPaddle $ Position (V2 1.75 0)
  newEntity (Constraint motA motB (SimpleMotor pi))

  cmap $ \(_ :: Shape) -> material

mousePos :: (Float, Float) -> System World (V2 Double)
mousePos mouseWin = do
  cam <- get global
  return . fmap realToFrac $ windowToWorld cam mouseWin

handle :: Event -> System World ()
handle (EventMotion mouseWin) = do
  mpos <- mousePos mouseWin
  cmap $ \MouseBody -> Position mpos

handle (EventKey (MouseButton LeftButton) Down _ mouseWin) = do
  mpos <- mousePos mouseWin
  pq <- pointQuery mpos 0 collisionFilter
  forM_ pq $ \(PointQueryResult shape _ _ _) -> do
    Shape otherEty _ <- get shape
    mouse <- newEntity (MouseBody, StaticBody, Position mpos)
    newEntity (Constraint mouse otherEty (PivotJoint mpos), MaxForce 2)

handle (EventKey (MouseButton LeftButton) Up _ _) =
  cmap $ \MouseBody -> Not :: Not (MouseBody, Body)

handle (EventKey (MouseButton RightButton) Down _ mouseWin) = do
  mpos <- mousePos mouseWin
  box <- newEntity (DynamicBody, Position mpos)
  newEntity (Shape box (cRectangle 0.3), material)
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
