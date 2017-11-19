{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Apecs.Physics
import           Apecs.Physics.Render

data Player = Player
instance Component Player where
  type Storage Player = Unique Player

makeWorld "World" [''Color, ''Physics, ''Player]

walls    = 0
player   = 1
platform = 2

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( DynamicBody
            , Position (V2 0 (-3))
            , Shape (Circle 0 0.5) defaultProperties {friction = 1, collisionType = player}
            , Player )

  newEntity ( StaticBody
            , hollowBox 12 8 0 defaultProperties {friction = 1, collisionType = walls})

  newEntity ( StaticBody
            , Shape (Segment (V2 (-2) 0) (V2 2 0) 0) defaultProperties {friction = 1, collisionType = walls})

  newEntity ( StaticBody
            , Position (V2 0 (-1.5))
            , red
            , Shape (Segment (V2 (-2) 0) (V2 2 0) 0) defaultProperties {friction = 1, collisionType = platform})

  cb <- mkBeginCB oneWayPlatform
  newEntity$ defaultHandler { source  = Between player platform
                            , beginCB = Just cb }

oneWayPlatform (Collision (V2 _ y) _ _) = return (y < 0.7)

handleEvent (EventKey _ Down _ _) = rmap $ \Player -> Velocity (V2 0 9)
handleEvent _                     = return ()

main = playWorld (InWindow "helloworld" (640,480) (10,10)) 40 initWorld handleEvent initialize
