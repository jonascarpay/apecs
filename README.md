# apecs
#### a fast, type driven, extensible ECS in pure Haskell

apecs is an Entity Component System inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
It aims to provide Haskell's expressivity and safety, without sacrificing performance or extensibility.
Apecs distinguishes itself from other Haskell ECS by focusing on mutable data structures.


### Design
The library mostly provides an interface to the `SStorage m s` type class, which defines a mutable component store `s`, that lives in some monad `m`.
  * We define a Component by associating it with some instance of `SStorage IO`
  * A System is a ReaderT; your game is a `System w IO a` where `w` is your world. Your world is immutable and after initialization only holds references to mutable data structures.
  * By defining instances for tuples we can compose components, allowing us to work with e.g. `(Position, Velocity)` as if it were a single component.
  * Defining instances for `SStorage STM` allows you to run atomically run systems in parallel*
  * Different storages can define different read/write types e.g. set a flag by writing a Bool, or delete a Velocity component with `Writes Nothing :: Writes Velocity`.
  * Most systems can be defined by a pure operation e.g. turning `stepVelocity :: (Position, Velocity) -> Position` into an STM transaction that is applied to all entities with both a Position and a Velocity.

### Performance
Using the [ecs-bench](https://github.com/lschmierer/ecs_bench) pos_vel benchmark to compare apecs to specs gives me the following results on my machine:

|     | specs | apecs |
| --- | ----- | --- |
| build | 688 us | 332 us | 
| update | 31 us | 56 us |

The main reason apecs can even keep up with specs, which was written in _Rust_ mind you, is caching.
Wrapping a `SStorage` in a `Cached` adds a layer of caching to the store.
Because every component is addressed by an `Entity`, we can keep the most recent reads and writes in a fixed-length vector.

Consider this a proof of concept.
The API is still under heavy development, and there is little documentation outside this write-up.
Still, contributions are very welcome!

### Example
```haskell
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
  -- Create new entities
  newEntityWith (Writes (Just (Position 0),   Just (Velocity 3)) :: Writes (Position, Velocity))
  newEntityWith (Writes (Just (Position 1.5), Just (Velocity 9)) :: Writes (Position, Velocity))

  newEntityWith (Writes (Just (Velocity 0)) :: Writes Velocity)
  newEntityWith (Writes (Just (Position 0)) :: Writes Position)

  -- This next line does not type-check, because World does not have the component Enemy
  -- newEntityWith (Writes (Just (Position 3), True) :: Writes (Position, Enemy))

  -- Print the positions of all objects
  E.mapM_ printPositions

  -- Add velocities to positions. The type signature of stepVelocity is sufficient for mapR
  -- to only focus on entities that have both
  mapR stepVelocity

  E.mapM_ printPositions

stepVelocity :: Elem (Velocity, Position) -> Writes Position
stepVelocity (Elem (Velocity v, Position p)) = Writes $ Just (Position (v+p))

printPositions :: (Entity a, Elem Position) -> IO ()
printPositions (Entity e, Elem p) = putStrLn ("Entity " ++ show e ++ " has position " ++ show p)



main = do w <- liftM3 World empty empty empty
          runSystem game w
```
