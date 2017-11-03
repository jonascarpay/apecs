{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

import Apecs
import Apecs.Stores (Cache)
import Apecs.Concurrent (prmap)
import Linear

newtype Position = Position (V2 Double) deriving Show
-- Turn Position into a component by specifiying the type of its Storage
instance Component Position where
  -- The simplest store is a Map
  type Storage Position = Map Position


newtype Velocity = Velocity (V2 Double)
instance Component Velocity where
  -- We can add a Cache for faster access
  type Storage Velocity = Cache 100 (Map Velocity)

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  -- Unique contains at most one component. See the Stores module.
  type Storage Player = Unique Player

-- Generate a world type and instances
makeWorld "World" [''Position, ''Velocity, ''Player]

game :: System World ()
game = do
  -- Create new entities
  ety <- newEntity (Position 0)
  -- Components can be composed using tuples
  newEntity (Position 0, Velocity 1)
  newEntity (Position 1, Velocity 1, Player)

  -- set (over)writes components
  set ety (Velocity 2)

  let stepVelocity (Position p, Velocity v) = Position (v+p)

  -- Side effects
  liftIO$ putStrLn "Stepping velocities"
  -- rmap maps a pure function over all entities in its domain
  rmap stepVelocity
  -- prmap n does the same, but in parallel
  prmap 2 stepVelocity

  -- Print all positions
  cmapM_ $ \(Position p) -> liftIO (print p)

main :: IO ()
main = initWorld >>= runSystem game
