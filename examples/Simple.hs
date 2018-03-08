{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Apecs.Stores
import Apecs.Types
import Linear

newtype Position = Position (V2 Double) deriving Show
-- To declare a component, we need to specify how to store it
instance Component Position where
  type Storage Position = Map Position -- The simplest store is a Map

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity) -- Caches allow fast access

data Player = Player deriving Show -- A single constructor component for tagging the player
instance Component Player where
  type Storage Player = Unique Player -- Unique contains at most one component

data Flying = Flying
instance Component Flying where
  type Storage Flying = Map Flying

makeWorld "World" [''Position, ''Velocity, ''Player, ''Flying] -- Generate World and instances

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 1, Velocity 1, Player)
  newEntity (Position 1, Velocity 2, Flying)

  cmap   $ \(Position p, Velocity v) -> Position (v+p)

  cmapM_ $ \(Position _, Entity e, p :: Maybe Player) -> liftIO . print $ (e, p) -- Print player velocity

main :: IO ()
main = initWorld >>= runSystem game
