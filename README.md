# apecs
##### [hackage](https://hackage.haskell.org/package/apecs) | [documentation](https://hackage.haskell.org/package/apecs/docs/Apecs.html) | [apecs-physics](https://github.com/jonascarpay/apecs-physics)

apecs is a framework for high-performance game programming in Haskell.
At its core is an _Entity Component System_ inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
This repository contains the core ECS, which can be extended as desired.
For example, [apecs-physics](https://github.com/jonascarpay/apecs-physics) is a 2D physics and rendering module.

There is a code example below, and a basic tutorial can be found [here](https://github.com/jonascarpay/apecs/blob/master/tutorials/RTS.md).
For a general introduction to ECS, see [this talk](https://www.youtube.com/watch?v=lNTaC-JWmdI&feature=youtu.be&t=218) or [here](https://en.wikipedia.org/wiki/Entity–component–system).

### Performance
Performance is good.
Running [ecs-bench](https://github.com/lschmierer/ecs_bench) shows that apecs is competitive with the fastest Rust ECS frameworks.

|               | pos_vel build | pos_vel step | parallel build | parallel step |
| ------------- | ------------- | ------------ | -------------- | ------------- |
| apecs         | 239           | 34           | 777            | 371           |
| calx          | 261           | 21           | 442            | 72            |
| constellation | 306           | 10           | 567            | 256           |
| froggy        | 594           | 13           | 1565           | 97            |
| specs         | 753           | 38           | 1016           | 205           |

![Benchmarks](/bench/chart.png)

There is a performance guide [here](https://github.com/jonascarpay/apecs/blob/master/tutorials/GoingFast.md).

### Example
```haskell
import Apecs
import Apecs.Stores (Cache)
import Apecs.Concurrent (prmap)
import Linear

newtype Position = Position (V2 Double) deriving Show
-- Turn Position into a component by specifiying the type of its Storage
instance Component Position where
  -- The simplest store is a Map
  type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double)
instance Component Velocity where
  -- We can add a Cache for faster access
  type Storage Velocity = Cache 100 (Map Velocity)

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  -- Unique contains at most one component. See the Stores module.
  type Storage Player = Unique Player

-- Generate a world type and instances
makeWorld "World" [''Position, ''Velocity, ''Player]

game :: System World ()
game = do
  -- Create new entities
  ety <- newEntity (Position 0)
  -- Components can be composed using tuples
  newEntity (Position 0, Velocity 1)
  newEntity (Position 1, Velocity 1, Player)

  -- set (over)writes components
  set ety (Velocity 2)

  let stepVelocity (Position p, Velocity v) = Position (v+p)

  -- Side effects
  liftIO$ putStrLn "Stepping velocities"
  -- rmap maps a pure function over all entities in its domain
  rmap stepVelocity
  -- prmap n does the same, but in parallel
  prmap 2 stepVelocity

  -- Print all positions
  cmapM_ $ \(Position p) -> liftIO (print p)

main :: IO ()
main = initWorld >>= runSystem game
```
