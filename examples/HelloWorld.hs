{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Apecs.Physics
import           Apecs.Physics.Render

makeWorld "World" [''Color, ''Physics]

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( DynamicBody
            , Position (V2 0 2)
            , Shape (Circle 0 0.5) defaultProperties {friction = 1} )

  newEntity ( StaticBody
            , Shape (Segment (V2 (-3) 0) (V2 3 0) 0) defaultProperties {friction = 1}
            , Angle (-pi/10) )
  return ()

main = simulateWorld (InWindow "helloworld" (640,480) (10,10)) 40 initWorld initialize
