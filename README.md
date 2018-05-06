# apecs
[![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)
[![Hackage](https://img.shields.io/hackage/v/apecs.svg)](https://hackage.haskell.org/package/apecs)
[![apecs on Stackage LTS 11](http://stackage.org/package/apecs/badge/lts-11)](http://stackage.org/lts-10/package/apecs)

apecs is an _Entity Component System_ framework inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
The front-end DSL allows you to concisely express your game logic, with an efficient back-end storage framework that can be extended to meet any performance needs.

#### Links
- [documentation](https://hackage.haskell.org/package/apecs/docs/Apecs.html)
- [introductory tutorial](https://github.com/jonascarpay/apecs/blob/master/tutorials/RTS.md)
- [performance guide](https://github.com/jonascarpay/apecs/blob/master/tutorials/GoingFast.md)
- [apecs-physics](https://github.com/jonascarpay/apecs-physics)

#### Performance
[ecs-bench](https://github.com/lschmierer/ecs_bench) shows that apecs is competitive with the fastest Rust ECS frameworks.

![Benchmarks](/bench/chart.png)

#### Example
```haskell
{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Apecs.Stores
import Apecs.Core
import Linear

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
