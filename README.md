# apecs
##### [hackage](https://hackage.haskell.org/package/apecs) | [documentation](https://hackage.haskell.org/package/apecs/docs/Apecs.html) | [tutorials](https://github.com/jonascarpay/apecs/blob/master/tutorials/)

apecs is an Entity Component System inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
It exposes a DSL that translates to fast storage operations, resulting in expressivity without sacrificing performance or safety.

There is an example below, and a tutorial can be found [here](https://github.com/jonascarpay/apecs/blob/master/tutorials/RTS.md).
For a general introduction to ECS, see [this talk](https://www.youtube.com/watch?v=lNTaC-JWmdI&feature=youtu.be&t=218) or [here](https://en.wikipedia.org/wiki/Entity–component–system).

### Performance
Performance is good.
Running the [ecs-bench](https://github.com/lschmierer/ecs_bench) pos_vel benchmark shows that we can keep up with specs, which was written in Rust:

|        | specs  | apecs  |
| ------ | ------ | ------ |
| build  | 699 us | 285 us | 
| update | 34 us  | 46 us  |

There is a performance guide [here](https://github.com/jonascarpay/apecs/blob/master/tutorials/GoingFast.md).

### Example
```haskell
import Apecs
import Apecs.TH (makeWorld)
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
