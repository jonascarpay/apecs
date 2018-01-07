{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses #-}

import Apecs
import Linear

newtype Position = Position (V2 Double) deriving Show
-- To declare a component, we need to specify how to store it
instance Component Position where
  type Storage Position = Map Position -- The simplest store is a Map

newtype Velocity = Velocity (V2 Double)
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity) -- Caches allow fast access

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  type Storage Player = Unique Player -- Unique contains at most one component

makeWorld "World" [''Position, ''Velocity, ''Player] -- Generate World and instances

game :: System World ()
game = do
  ety <- newEntity (Position 0) -- new entity with just a Position
  newEntity (Position 1, Velocity 1, Player) -- Tuples for easy composition
  set ety (Velocity 2) -- set (over)writes components

  -- rmap maps a pure function over all entities in its domain. prmap does the same, but in parallel
  rmap $ \(Position p, Velocity v) -> Position (v+p)

  cmapM_ $ \(Position p) -> liftIO (print p) -- Print all positions
  cmapM_ $ \(Player, Velocity v) -> liftIO (print v) -- Print player velocity

main :: IO ()
main = initWorld >>= runSystem game
