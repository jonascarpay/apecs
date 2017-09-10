{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Control.Monad

import Apecs as E
import Apecs.Stores
import Apecs.Vector -- Optional module for basic 2D and 3D vectos

newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity)

newtype Position = Position (V2 Double) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cache 100 (Map Position)

data Enemy -- Flags enemies
instance Component Enemy where
  type Storage Enemy = Set Enemy


-- Boilerplate starts here
data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , entityCounter :: Storage EntityCounter }

instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Velocity      where getStore = System $ asks velocities
instance World `Has` EntityCounter where getStore = System $ asks entityCounter
-- Boilerplate ends here

type System' a = System World a

game :: System' ()
game = do
  -- Create four new entities
  newEntity (Position 0, Velocity 1)
  newEntity (Position 1, Velocity 1)

  newEntity (Velocity 0)
  newEntity (Position 0)

  -- This next line does not type-check, because World does not have the component Enemy
  -- newEntity (Writes (Just (Position 3), True) :: Writes (Position, Enemy))

  printPositions
  liftIO$ putStrLn "Stepping velocities"
  rmap' stepVelocity
  printPositions

  -- We can explicitly invoke the garbage collector to keep performance predictable
  runGC

-- mapR is used to apply a pure function to all entities that have the required components.
stepVelocity :: (Velocity, Position) -> Position
stepVelocity (Velocity v, Position p) = Position (v+p)

-- We can similarly iterate over all valid entities with some system
printPositions :: System' ()
printPositions = do slice :: Slice Position <- sliceAll
                    sliceMapM_ f slice
                    liftIO.putStrLn$ show (sliceSize slice) ++ " total positions"
  where
    f :: Entity Position -> System' ()
    f e = do
      Safe (Just p) <- get e
      liftIO$ putStrLn (show e ++ " has position " ++ show p)


main :: IO ()
main = do w <- liftM3 World (initStore ()) (initStore ()) initCounter
          runSystem game w
