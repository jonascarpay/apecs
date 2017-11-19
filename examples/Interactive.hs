{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where


import           Apecs
import           Control.Monad
import           System.Random

import           Apecs.Physics
import           Apecs.Physics.Render

data Box = Box
instance Component Box where
  type Storage Box = Unique Box

makeWorld "World" [''Box, ''Color, ''Physics]

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( Box
            , KinematicBody
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

handleEvent (EventKey (SpecialKey KeyLeft)  Down _ _) = rmap $ \Box -> AngularVelocity (pi/6)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = rmap $ \Box -> AngularVelocity (-pi/6)
handleEvent (EventKey (SpecialKey KeyDown)  Down _ _) = rmap $ \Box -> AngularVelocity 0
handleEvent _ = return ()

main = playWorld (InWindow "tumbler" (640,480) (10,10)) 10 initWorld handleEvent initialize
