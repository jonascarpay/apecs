{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad

import           Apecs.Physics
import           Apecs.Physics.Render

makeWorld "World" [''Color, ''Physics]

shape sh = Shape sh defaultProperties {friction = 1, elasticity = 0.1}

initialize = do
  setGlobal $ Gravity (V2 0 (-10))

  {-forM_ [-2 .. 2] $ \x ->-}
  forM_ [-2 , 2] $ \x ->
    newEntity (StaticBody, Shape (Segment (V2 x (-1.5)) (V2 x 1.5) 0) defaultProperties {elasticity = 0.1, friction = 1})

  {-forM_ [-1.5, -0.5, 0.5, 1.5] $ \y ->-}
  forM_ [-1.5, 1.5] $ \y ->
    newEntity (StaticBody, shape$ Segment (V2 (-2) y) (V2 2 y) 0)

  -- PivotJoint
  newEntity (DynamicBody, shape$ Circle 0 0.1, Position (V2 (-1.5) (-1)) )
  newEntity (DynamicBody, shape$ Circle 0 0.1, Position (V2 (-1.501) (-1.2)) )

stepper = return ()
handler _ = return ()

main = playWorld (InWindow "constraint gallery" (640,480) (10,10)) 150 initWorld initialize handler stepper
