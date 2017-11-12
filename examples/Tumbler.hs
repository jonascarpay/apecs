{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad
import           System.Random

import           Apecs.Physics
import           Apecs.Physics.Render

-- TODO: enforce?:
--    Cannot set mass of non-dynamic body
--    Cannot simulate when mass <= 0
--    Cannot simulate when moment <= 0

makeWorld "World" [''Color, ''Physics]

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( KinematicBody
            , AngularVelocity (-pi/6)
            , hollowBox 30 30 0 defaultProperties )

  replicateM_ 300 $ do
    x      <- liftIO$ randomRIO (-9,9)
    y      <- liftIO$ randomRIO (-9,9)
    radius <- liftIO$ randomRIO (0.4,0.8)
    let color = (realToFrac x+9)/19

    newEntity ( DynamicBody
              , Shape (Circle 0 radius) defaultProperties {elasticity=0.9, friction=1}
              , Position (V2 x y)
              , makeColor 1 color color 1 )

main = simulateWorld (InWindow "tumbler" (640,480) (10,10)) 10 initWorld initialize
