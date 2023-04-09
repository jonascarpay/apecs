# apecs

apecs is an [_Entity Component System_](https://en.wikipedia.org/wiki/Entity_component_system) (ECS) library for game development.

apecs aims to be
* **Fast** - Performance is competitive with Rust ECS libraries (see benchmark results below)
* **Safe** - Completely hides the dangers of the low-level machinery
* **Concise** - Game logic is expressed using a small number of powerful combinators
* **Flexible** - Easily add new modules or backends
* **Cool**

![Benchmarks](https://raw.githubusercontent.com/jonascarpay/apecs/master/apecs/bench/chart.png)

### Links
- [documentation on hackage](https://hackage.haskell.org/package/apecs/docs/Apecs.html)
- [tutorial](https://github.com/jonascarpay/apecs/blob/master/examples/Shmup.md) and other [examples](https://github.com/jonascarpay/apecs/tree/master/examples)
- community chat at [`#apecs` on the haskell gamedev discord](https://discord.gg/vxpWtBA) or [`#haskell-game:matrix.org`](https://matrix.to/#/#haskell-game:matrix.org)

##### Games/articles
- [Notakto](https://github.com/Ashe/Notakto/), and [associated blog post/apecs tutorial](https://aas.sh/blog/notakto-a-haskell-game-with-apecs-and-raylib/) by [@Ashe](https://github.com/Ashe)
- [mallRL](https://github.com/nmaehlmann/mallRL) - a _grocery shopping roguelike_ by [@nmaehlmann](https://github.com/nmaehlmann)
- [An implementation of the Unity tutorial project using apecs](https://github.com/mewhhaha/apecs-unity-tutorial-haskell) by [@mewhhaha](https://github.com/mewhhaha)
- [SpaceMar](https://gitlab.com/dpwiz/spacemar) by [@dpwiz](https://gitlab.com/dpwiz)
- [Achtung die haskell](https://github.com/mewhhaha/achtung-die-haskell) by [@mewhhaha](https://github.com/mewhhaha)
- [(Your game here)](https://github.com/jonascarpay/apecs/pulls)

##### Packages
- [apecs](https://github.com/jonascarpay/apecs/tree/master/apecs)

  [![Hackage](https://img.shields.io/hackage/v/apecs.svg)](https://hackage.haskell.org/package/apecs)
  [![Stackage](https://www.stackage.org/package/apecs/badge/lts?label=lts)](https://www.stackage.org/package/apecs)
  [![Stackage](https://www.stackage.org/package/apecs/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs)
  [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

- [apecs-physics](https://github.com/jonascarpay/apecs/tree/master/apecs-physics) - 2D physics using the [Chipmunk2D](https://github.com/slembcke/Chipmunk2D) engine

  [![Hackage](https://img.shields.io/hackage/v/apecs-physics.svg)](https://hackage.haskell.org/package/apecs-physics)
  [![Stackage](https://www.stackage.org/package/apecs-physics/badge/lts?label=lts)](https://www.stackage.org/package/apecs-physics)
  [![Stackage](https://www.stackage.org/package/apecs-physics/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs-physics)
  [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

- [apecs-gloss](https://github.com/jonascarpay/apecs/tree/master/apecs-gloss) - Simple frontend for [gloss](http://hackage.haskell.org/package/gloss)-based rendering

  [![Hackage](https://img.shields.io/hackage/v/apecs-gloss.svg)](https://hackage.haskell.org/package/apecs-gloss)
  [![Stackage](https://www.stackage.org/package/apecs-gloss/badge/lts?label=lts)](https://www.stackage.org/package/apecs-gloss)
  [![Stackage](https://www.stackage.org/package/apecs-gloss/badge/nightly?label=nightly)](https://www.stackage.org/package/apecs-gloss)
  [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

- [apecs-stm](https://github.com/jonascarpay/apecs/tree/master/apecs-stm) - STM-based stores for easy concurrency

  [![Hackage](https://img.shields.io/hackage/v/apecs-stm.svg)](https://hackage.haskell.org/package/apecs-stm)
  [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

- [*examples*](https://github.com/jonascarpay/apecs/tree/master/examples/)

  [![Build Status](https://travis-ci.org/jonascarpay/apecs.svg?branch=master)](https://travis-ci.org/jonascarpay/apecs)

### Example
```haskell
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Apecs
import Linear (V2 (..))

newtype Position = Position (V2 Double) deriving Show
newtype Velocity = Velocity (V2 Double) deriving Show
data Flying = Flying

makeWorldAndComponents "World" [''Position, ''Velocity, ''Flying]

game :: System World ()
game = do
  newEntity (Position 0, Velocity 1)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  cmap $ \(Position p, Velocity v) -> Position (v+p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - V2 0 1)
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)

main :: IO ()
main = initWorld >>= runSystem game
```
