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

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( DynamicBody
            , Position (V2 0 1.5)
            , Shape (Circle 0 0.5) defaultProperties {friction = 1, collisionType = 1}
            , Player )

  newEntity ( StaticBody
            , Shape (Segment (V2 (-3) 0) (V2 3 0) 0) defaultProperties {friction = 1, collisionType = 2})

  newEntity ( StaticBody
            , Position (V2 0 3)
            , Shape (Segment (V2 (-2) 0) (V2 2 0) 0) defaultProperties {friction = 1, collisionType = 2})

  cb <- makeCallback collisionBegin
  newEntity ( CollisionHandler 1 2 (Just cb) Nothing)

collisionBegin (CollisionPair _ entA _) = do
  Safe (Just (Velocity (V2 _ y))) :: Safe Velocity <- get (cast entA)
  liftIO$ putStrLn "Callback!"
  return (y<=0)

handleEvent (EventKey _ Down _ _) = rmap $ \(_ :: Player) -> Velocity (V2 0 9)
handleEvent _                     = return ()

main = playWorld (InWindow "helloworld" (640,480) (10,10)) 40 initWorld handleEvent initialize
