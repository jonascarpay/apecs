{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Control.ECS as E
import Control.ECS.Vector -- There is a default lightweight vector module
import Control.Monad

type Vec = V2 Double

newtype Velocity = Velocity Vec deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cached (Map Velocity)

newtype Position = Position Vec deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cached (Map Position)

data Enemy -- Flags enemies
instance Component Enemy where
  type Storage Enemy = FlagSet


-- Boilerplate starts here
data World = World
  { positions     :: Store Position
  , velocities    :: Store Velocity
  , entityCounter :: Store EntityCounter }

instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Velocity      where getStore = System $ asks velocities
instance World `Has` EntityCounter where getStore = System $ asks entityCounter
-- Boilerplate ends here


game :: System World IO ()
game = do
  -- Create four new entities
  newEntityWith (Writes (Just (Position zero),       Just (Velocity (V2 3 3))) :: Writes (Position, Velocity))
  newEntityWith (Writes (Just (Position (V2 1.5 0)), Just (Velocity (V2 0 9))) :: Writes (Position, Velocity))

  newEntityWith (Writes (Just (Velocity zero)) :: Writes Velocity)
  newEntityWith (Writes (Just (Position zero)) :: Writes Position)

  -- This next line does not type-check, because World does not have the component Enemy
  -- newEntityWith (Writes (Just (Position 3), True) :: Writes (Position, Enemy))

  -- Print the positions of all objects
  E.mapM_ printPositions

  -- Add velocities to positions. The type signature of stepVelocity is sufficient for mapR
  -- to only focus on entities that have both
  mapR stepVelocity

  E.mapM_ printPositions
  runGC

stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity v, Position p)) = Writes $ Just (Position (v.+p))

printPositions :: (Entity a, Elem Position) -> IO ()
printPositions (Entity e, Elem p) = putStrLn ("Entity " ++ show e ++ " has position " ++ show p)



main = do w <- liftM3 World empty empty empty
          runSystem game w
