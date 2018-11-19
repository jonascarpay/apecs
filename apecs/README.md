# apecs

apecs is an _Entity Component System_ (ECS) framework inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
ECS presents a data-driven approach to game development, that elegantly tackles many of the unique issues of game programming.
apecs aims to be
* **Fast** - apecs is designed for high-performance applications. Its performance is competitive with Rust ECS libraries.
* **Simple** - Game logic is expressed using a small number of combinators, and minimal boilerplate.
* **Safe** - The `cmap`/`cfold`-DSL hides all the dangers of the low-level API.
* **Extensible** - apecs can be used with anything that implements the low-level API. See [apecs-physics](apecs-physics/) or [apecs-stm](apecs-stm/) for examples.

#### Links
- [manual](https://github.com/jonascarpay/apecs/blob/master/prepub.pdf) (see [#19](https://github.com/jonascarpay/apecs/issues/19))
- [tutorial](https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md)
- [documentation](https://hackage.haskell.org/package/apecs/docs/Apecs.html)
- [apecs-physics](https://github.com/jonascarpay/apecs-physics)

#### Performance
[ecs-bench](https://github.com/lschmierer/ecs_bench) shows that apecs is competitive with the fastest Rust ECS frameworks.

![Benchmarks](bench/chart.png)

#### Example
```haskell
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Linear (V2 (..))

newtype Position = Position (V2 Double) deriving Show
-- To declare a component, we need to specify how to store it
instance Component Position where
  type Storage Position = Map Position -- The simplest store is a Map

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity) -- Caching adds fast reads/writes

data Flying = Flying
instance Component Flying where
  type Storage Flying = Map Flying

makeWorld "World" [''Position, ''Velocity, ''Flying] -- Generate World and instances

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  -- Add velocity to position
  cmap $ \(Position p, Velocity v) -> Position (v+p)
  -- Apply gravity to non-flying entities
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - (V2 0 1))
  -- Print a list of entities and their positions
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

main :: IO ()
main = initWorld >>= runSystem game
```
