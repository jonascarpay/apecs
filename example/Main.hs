{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Control.Monad

import Control.ECS as E
import Control.ECS.Vector -- Optional module for basic 2D and 3D vectos

newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cache (Map Velocity)

newtype Position = Position (V2 Double) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cache (Map Position)

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

type System' a = System World a

game :: System' ()
game = do
  -- Create four new entities
  newEntityWith (Writes (Just (Position 0), Just (Velocity 1)) :: Writes (Position, Velocity))
  newEntityWith (Writes (Just (Position 1), Just (Velocity 1)) :: Writes (Position, Velocity))

  newEntityWith (Writes (Just (Velocity 0)) :: Writes Velocity)
  newEntityWith (Writes (Just (Position 0)) :: Writes Position)

  -- This next line does not type-check, because World does not have the component Enemy
  -- newEntityWith (Writes (Just (Position 3), True) :: Writes (Position, Enemy))

  printPositions
  liftIO$ putStrLn "Stepping velocities"
  apply stepVelocity
  printPositions

  -- We can explicitly invoke the garbage collector to keep performance predictable
  runGC

-- mapR is used to apply a pure function to all entities that have the required components.
stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity v, Position p)) = Writes $ Just (Position (v+p))

-- We can similarly iterate over all valid entities with some system
printPositions :: System' ()
printPositions = do slice :: Slice Position <- E.all
                    E.mapM_ f slice
                    liftIO.putStrLn$ show (size slice) ++ " total positions"
  where
    f :: (Entity a, Reads Position) -> System' ()
    f (Entity e, Reads p) = liftIO$ putStrLn ("Entity " ++ show e ++ " has position " ++ show p)


main = do w <- liftM3 World empty empty empty
          runSystem game w
