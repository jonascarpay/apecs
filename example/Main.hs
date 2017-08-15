{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Control.ECS as E
import Control.Monad

newtype Velocity = Velocity Float deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cached (Map Velocity)

newtype Position = Position Float deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cached (Map Position)

data Enemy -- Flags enemies
instance Component Enemy where
  type Storage Enemy = FlagSet


data World = World
  { positions     :: Store Position
  , velocities    :: Store Velocity
  , entityCounter :: Store EntityCounter }

instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Velocity      where getStore = System $ asks velocities
instance World `Has` EntityCounter where getStore = System $ asks entityCounter

game :: System World IO ()
game = do
  -- Create three new entities
  newEntityWith (Writes (Just (Position 0),   Just (Velocity 3)) :: Writes (Position, Velocity))
  newEntityWith (Writes (Just (Position 1.5), Just (Velocity 9)) :: Writes (Position, Velocity))

  newEntityWith (Writes (Just (Velocity 0)) :: Writes Velocity)

  -- This next line does not type-check, because World does not have the component Enemy
  -- newEntityWith (Writes (Just (Position 3), True) :: Writes (Position, Enemy))

  -- Print the positions of all objects
  E.mapM_ printPositions

  -- Step all velocities. The type signature of stepVelocity is sufficient
  mapR stepVelocity

  E.mapM_ printPositions

stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity v, Position p)) = Writes $ Just (Position (v+p))

printPositions :: (Entity a, Elem Position) -> IO ()
printPositions (Entity e, Elem p) = putStrLn ("Entity " ++ show e ++ " has position " ++ show p)

main = do w <- liftM3 World empty empty empty
          runSystem game w
