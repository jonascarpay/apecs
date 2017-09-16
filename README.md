# apecs

apecs is an Entity Component System inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
It exposes a DSL that translates to fast storage operations, resulting in expressivity without sacrificing performance or safety.

There is an example below, and a tutorial can be found [here](https://github.com/jonascarpay/apecs/blob/master/tutorials/RTS.md).
For a general introduction to ECS, see [this talk](https://www.youtube.com/watch?v=lNTaC-JWmdI&feature=youtu.be&t=218) or [here](https://en.wikipedia.org/wiki/Entity–component–system).

### Performance
Performance is good.
Running the [ecs-bench](https://github.com/lschmierer/ecs_bench) pos_vel benchmark shows that we can keep up with specs, which was written in Rust:

|     | specs | apecs |
| --- | ----- | --- |
| build | 699 us | 285 us | 
| update | 34 us | 46 us |

### Example
```haskell
import Apecs
import Apecs.Stores
import Apecs.Util
import Linear

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

instance Flag Enemy where flag = Enemy
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
  ety <- newEntity (Position 0)
  -- Components can be composed using tuples
  newEntity (Position 0, Velocity 1)
  -- Tagging one as an enemy is a matter of adding the constructor
  newEntity (Position 1, Velocity 1, Enemy)

  -- Side effects
  liftIO$ putStrLn "Stepping velocities"
  -- rmap maps a pure function over all entities in its domain
  rmap $ \(Position p, Velocity v) -> Position (v+p)

  -- Set can be used to (over)write components
  set ety (Position 2, Enemy)

  -- Print the positions of all enemies
  cmapM_ $ \(Enemy, Position p) -> liftIO (print p)

main :: IO ()
main = do w <- World <$> initStore <*> initStore <*> initStore <*> initCounter
          runSystem game w
```
