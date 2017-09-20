## apecs tutorial
### ECS Basics

This is the second in a series of tutorials on apecs.
The focus of this tutorials is ECS semantics.
You are highly encouraged to follow along in GHCI.
To do so, clone this repo, run `stack repl`, and paste the following code into the GHCI prompt:
```haskell
:set -XTypeFamilies -XScopedTypeVariables -XMultiParamTypeClasses -XFlexibleContexts -XFlexibleInstances

import Apecs
import Apecs.Stores
import Apecs.Util

newtype Velocity = Velocity Int deriving (Eq, Show)
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Position = Position Float deriving (Eq, Show)
instance Component Position where type Storage Position = Map Position

data Player = Player deriving (Eq, Show)
instance Component Player where type Storage Player = Unique Player

data World = World {velocities :: Storage Velocity, positions :: Storage Position, players :: Storage Player, ec :: Storage EntityCounter}
instance Has World Velocity where getStore = System $ asks velocities
instance Has World Position where getStore = System $ asks positions
instance Has World Player where getStore = System $ asks players
instance Has World EntityCounter where getStore = System $ asks ec
type System' a = System World a

w <- World <$> initStore <*> initStore <*> initStore <*> initCounter
run = runWith w

printPositions = run$ (cimapM_ (\(e,p :: Position) -> liftIO $ print (e,p)) :: System' ())
printVelocities = run$ (cimapM_ (\(e,p :: Velocity) -> liftIO $ print (e,p)) :: System' ())
printPlayer = run$ (cimapM_ (\(e,p :: Player) -> liftIO $ print (e,p)) :: System' ())
printEntity e = run$ (do Safe r <- get e; liftIO (print r) :: System' ())

```
It defines a default world with components `Position`, `Velocity`, and `Player`.
`run` will allow us to run a system from the GHCI prompt, after which we can use the `print...` functions to show the current world state.

#### Introduction
The standard object oriented approach to game programming uses inheritance to structure its game logic.
Unfortunately, games are notoriously susceptible to the Deadly Diamond.
The most common solution is the [component pattern](http://gameprogrammingpatterns.com/component.html).
ECS are an extension of this, in which game logic is completely described in terms of loosely coupled components.

The reason for apecs is that while ECS have a lot of potential, their semantics are a poor fit for most programming languages.
This makes most ECS implementations verbose, even the terse ones.
Fortunately, Haskell is better at being an imperative language than most imperative languages.

#### The World
The `World`'s job is to store components.
Let's inspect the world we've been given.
Running `printPositions`, `printVelocities`, and `printPlayer` prints the contents of the three component stores our world has.
As expected, all are empty.
```
λ> printPositions

λ> printVelocities

λ> printPlayer

```

Let's add a new entity:
```
λ> run$ newEntity (Position 0)
Entity 0

λ> printPositions
(Entity 0,Position 0.0)
```
We created a new entity with a position of 0.
The world now tracks a `Position` component for `Entity 0`.
If you were to print the other component stores, you'd find that they still hold no components.

It's important to stress that this represents your entire game state.
A World only holds components, (usually) indexed by entities.

```
λ> run$ newEntity (Velocity 0, Position 0)
Entity 1

λ> printPositions
(Entity 0,Position 0.0)
(Entity 1,Position 0.0)

λ> printVelocities
(Entity 1,Velocity 0)
```
Here we see what it means for an entity to have both a `Velocity` and a `Position`;
there are two components that associate themselves with the same entity.

#### Entities
Let's track an entity.
```
λ> ety <- run$ newEntity (Velocity 1, Position 4)

λ> ety
Entity 2

λ> :t ety
ety :: Entity (Velocity, Position)

λ> Safe r <- run$ get ety
λ> r
(Just (Velocity 1),Just (Position 4.0))

λ> printEntity ety -- equivalent to the two lines above, used because Safe has no Show instance
(Just (Velocity 1),Just (Position 4.0))
```
At runtime, an entity really is just an integer, but at compile time, it also has a type, in this case `(Velocity, Position)`.
This type is generally interpreted as meaning what components we expect the entity to have.
`get`, for example, uses it to decide which components to look up.

The first entity we created only had a position.
As expected, reading its Velocity fails.
```
λ> printEntity (Entity 0 :: Entity (Position, Velocity))
(Just (Position 0.0),Nothing)
```

`destroy` uses the type of its argument to determine what components to destroy.
By casting `ety` to `Entity Velocity` we only delete its Velocity.
```
λ> run$ destroy (cast ety :: Entity Velocity)

λ> printEntity ety
(Nothing,Just (Position 4.0))

λ> printPositions
(Entity 0,Position 0.0)
(Entity 1,Position 0.0)
(Entity 2,Position 4.0)

λ> printVelocities
(Entity 1,Velocity 0)
```

`exists` tells us whether or not an entity has all components that its type suggests it has.
```
λ> run$ exists ety
False

λ> run$ exists (cast ety :: Entity Position)
True
```

`set` can be used to write or update a component.
It doesn't care about the type of its entity argument.
```
λ> run$ set ety (Velocity 2)

λ> printEntity ety
(Just (Velocity 2),Just (Position 4.0))
```

#### Conclusion
These are the primitive operations you'll find in most ECS.
You won't often use them directly, but almost all systems can be defined using only these primitives.
In fact, these are pretty close to the minimal complete definition for a `Store`.

Most stores will behave like the Maps our Positions and Velocities are stored in.
Sometimes, however, apecs will allow you to bend the rules a little when the performance boost is worth it  (try running `run$ set (Entity 0) Player >> set (Entity 1) Player` and then `printPlayer`).
See the documentation for what types of stores are available.

Next time, we'll make apecs fast.
