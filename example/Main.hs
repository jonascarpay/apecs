{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}

import Apecs
import Apecs.Stores
import Apecs.Util
import Apecs.Vector -- Optional module for basic 2D and 3D vectos

-- Component data definitions
newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
newtype Position = Position (V2 Double) deriving (Eq, Show)
data Enemy = Enemy -- A single constructor for tagging entites as enemies

-- Define Velocity as a component by giving it a storage type
instance Component Velocity where
  -- Store velocities in a cached map
  type Storage Velocity = Cache 100 (Map Velocity)

instance Component Position where
  type Storage Position = Cache 100 (Map Position)

instance Component Enemy where
  -- Because enemy is just a flag, we can use a set
  type Storage Enemy = Set Enemy

-- Define your world as containing the storages of your components
data World = World
  { positions     :: Storage Position
  , velocities    :: Storage Velocity
  , enemies       :: Storage Enemy
  , entityCounter :: Storage EntityCounter }

-- Define Has instances for components to allow type-driven access to their storages
instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Velocity      where getStore = System $ asks velocities
instance World `Has` Enemy         where getStore = System $ asks enemies
instance World `Has` EntityCounter where getStore = System $ asks entityCounter

type System' a = System World a

game :: System' ()
game = do
  -- Create new entities
  newEntity (Position 0)
  -- Components can be composed using tuples
  newEntity (Position 0, Velocity 1)
  -- Tagging one as an enemy is a matter of adding the constructor
  newEntity (Position 1, Velocity 1, Enemy)

  -- Side effects
  liftIO$ putStrLn "Stepping velocities"
  -- rmap maps a pure function over all entities in its domain
  rmap $ \(Position p, Velocity v) -> Position (v+p)

  -- slice performs a type-based query and returns a slice
  -- In this case, it produces a slice of all enemies
  -- mapMC_ iterates a system over a slice, providing both the entity and associated components
  slice All >>= mapMC_ printEnemyPosition

-- Define an EnemyUnit as a product of components
type EnemyUnit = (Enemy,Position)

-- Here we need to use a Safe type, because sliceMapMC_'s read might fail
printEnemyPosition :: (Entity EnemyUnit, Safe EnemyUnit) -> System' ()
printEnemyPosition (e,Safe (_,Just p)) = liftIO.putStrLn $ show e ++ " has " ++ show p

main :: IO ()
main = do w <- World <$> initStore <*> initStore <*> initStore <*> initCounter
          runSystem game w
