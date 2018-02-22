# apecs
[![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)
[![Hackage](https://img.shields.io/hackage/v/apecs.svg)](https://hackage.haskell.org/package/apecs)
[![apecs on Stackage LTS 9](http://stackage.org/package/apecs/badge/lts-9)](http://stackage.org/lts-9/package/apecs)
[![apecs on Stackage LTS 10](http://stackage.org/package/apecs/badge/lts-10)](http://stackage.org/lts-10/package/apecs)

apecs is an _Entity Component System_ inspired by [specs](https://github.com/slide-rs/specs) and [Entitas](https://github.com/sschmid/Entitas-CSharp).
It provides a collection of mutable component stores, and an expressive DSL for operating on those stores, both easily extended.

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
{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses, TemplateHaskell #-}

import Apecs
import Apecs.Stores
import Linear (V2)

newtype Position = Position (V2 Double) deriving Show
-- To declare a component, we need to specify how to store it
instance Component Position where
  type Storage Position = Map Position -- The simplest store is a Map

newtype Velocity = Velocity (V2 Double)
instance Component Velocity where
  type Storage Velocity = Cache 100 (Map Velocity) -- Caches allow fast access

data Player = Player -- A single constructor component for tagging the player
instance Component Player where
  type Storage Player = Unique Player -- Unique contains at most one component

makeWorld "World" [''Position, ''Velocity, ''Player] -- Generate World and instances

game :: System World ()
game = do
  ety <- newEntity (Position 0)              -- new entity with just a Position
  newEntity (Position 1, Velocity 1, Player) -- Tuples for easy composition
  set ety (Velocity 2)                       -- set (over)writes components

  -- rmap maps a pure function over all entities in its domain. prmap does the same, but in parallel
  rmap $ \(Position p, Velocity v) -> Position (v+p)

  cmapM_ $ \(Position p) -> liftIO (print p)         -- Print all positions
  cmapM_ $ \(Player, Velocity v) -> liftIO (print v) -- Print player velocity

main :: IO ()
main = initWorld >>= runSystem game
```
