{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Apecs as E
import Apecs.Stores
import Apecs.Vector -- Optional module for basic 2D and 3D vectos

newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity)

newtype Position = Position (V2 Double) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Cache 100 (Map Position)

data Enemy = Enemy -- Flags enemies
instance Component Enemy where
  type Storage Enemy = Set Enemy

-- Boilerplate
data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , enemies       :: Storage Enemy
  , entityCounter :: Storage EntityCounter }
instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Velocity      where getStore = System $ asks velocities
instance World `Has` Enemy         where getStore = System $ asks enemies
instance World `Has` EntityCounter where getStore = System $ asks entityCounter
type System' a = System World a

game :: System' ()
game = do
  -- Create four new entities
  newEntity (Position 0, Velocity 1)
  newEntity (Enemy, Position 1, Velocity 1)
  newEntity (Velocity 0)
  newEntity (Position 0)

  printPositions

  liftIO$ putStrLn "Stepping velocities"
  rmap' $ \(Velocity v, Position p) -> Position (v+p)

  printPositions

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
main = do w <- World <$> initStore () <*> initStore () <*> initStore () <*> initCounter
          runSystem game w
