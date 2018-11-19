# apecs [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

apecs is an _Entity Component System_ (ECS) framework inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
ECS presents a data-driven approach to game development, that elegantly tackles many of the unique issues of game programming.
apecs aims to be
* **Fast** - apecs is designed for high-performance applications. Its performance is competitive with Rust ECS libraries.
* **Simple** - Game logic is expressed using a small number of combinators, and minimal boilerplate.
* **Safe** - The `cmap`/`cfold`-DSL hides all the dangers of the low-level API.
* **Extensible** - apecs can be used with anything that implements the low-level API. See [apecs-physics](apecs-physics/) or [apecs-stm](apecs-stm/) for examples.

![Benchmarks](apecs/bench/chart.png)

#### Links
- [paper (prepublication)](apecs/prepub.pdf) (see [#19](https://github.com/jonascarpay/apecs/issues/19))
- [tutorial](examples/Shmup.md) and other [examples](examples/)
- [apecs-physics](apecs-physics/)
- [documentation](https://hackage.haskell.org/package/apecs/docs/Apecs.html)

#### Status
| Package | Hackage | Stack LTS | Stack Nightly |
|---|---|---|---|
| [apecs](apecs/) | [![Hackage](https://img.shields.io/hackage/v/apecs.svg)](https://hackage.haskell.org/package/apecs) | [![Stackage](https://www.stackage.org/package/apecs/badge/lts?label=lts)](https://www.stackage.org/package/apecs) | [![Stackage](https://www.stackage.org/package/apecs/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs)
| [apecs-physics](apecs-physics/) |  [![Hackage](https://img.shields.io/hackage/v/apecs-physics.svg)](https://hackage.haskell.org/package/apecs-physics) | [![Stackage](https://www.stackage.org/package/apecs-physics/badge/lts?label=lts)](https://www.stackage.org/package/apecs-physics) | [![Stackage](https://www.stackage.org/package/apecs-physics/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs-physics) |
| [apecs-physics-gloss](apecs-physics-gloss/) | [![Hackage](https://img.shields.io/hackage/v/apecs-physics-gloss.svg)](https://hackage.haskell.org/package/apecs-physics-gloss) | [![Stackage](https://www.stackage.org/package/apecs-physics-gloss/badge/lts?label=lts)](https://www.stackage.org/package/apecs-physics-gloss) | [![Stackage](https://www.stackage.org/package/apecs-physics-gloss/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs-physics-gloss) |
| [apecs-stm](apecs-stm/) | - | - | - |
| [examples](examples/) | - | - | - |

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
