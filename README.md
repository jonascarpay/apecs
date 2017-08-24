# apecs

apecs is an Entity Component System inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
It aims to provide Haskell's expressivity and safety, without sacrificing performance or extensibility.
It distinguishes itself from other Haskell ECS by focusing on mutable data structures.

For a general introduction to ECS, see [this talk](https://www.youtube.com/watch?v=lNTaC-JWmdI&feature=youtu.be&t=218) or [Wikipedia](https://en.wikipedia.org/wiki/Entity–component–system).

### Design
The library mostly provides an interface to the `SStorage m s` type class, which defines a mutable component store `s`, that lives in some monad `m`.
  * We define a Component by associating it with some instance of `SStorage IO`
  * A System is a ReaderT; your game is a `System w IO a` where `w` is your world. Your world is immutable and after initialization only holds references to mutable data structures.
  * By defining instances for tuples we can compose components, allowing us to work with e.g. `(Position, Velocity)` as if it were a single component.
  * Defining instances for `SStorage STM` allows you to atomically run systems in parallel*
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
Because every component is addressed by an `Entity`, which is essentially an int, we can make a buffer cache.
By choosing a sufficiently large cache we can add O(1) lookup/insertion/deletion to any data structure, and store most components in fixed-size vectors.

Consider this a proof of concept.
The API is still under heavy development, and there is little documentation outside this write-up.
Still, contributions are very welcome!

### Example
```haskell
import Control.ECS as E
import Control.ECS.Vector -- Optional module for basic 2D and 3D vectos
import Control.Monad

newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Cached (Map Velocity)

newtype Position = Position (V2 Double) deriving (Eq, Show)
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


type System' a = System World IO a

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
  stepVelocity
  printPositions

  -- We can explicitly invoke the garbage collector to keep performance predictable
  runGC

-- mapR is used to apply a pure function to all entities that have the required components.
stepVelocity :: System' ()
stepVelocity = mapR f
  where
    f :: Elem (Velocity, Position) -> Writes Position
    f (Elem (Velocity v, Position p)) = Writes $ Just (Position (v+p))

-- We can similarly iterate over all valid entities with some system
printPositions :: System' ()
printPositions = E.mapM_ f
  where
    f :: (Entity a, Elem Position) -> System' ()
    f (Entity e, Elem p) = lift$ putStrLn ("Entity " ++ show e ++ " has position " ++ show p)


main = do w <- liftM3 World empty empty empty
          runSystem game w
```
