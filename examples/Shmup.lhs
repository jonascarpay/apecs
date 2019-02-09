This document breaks down a small game written using apecs.
We're going to be making a little shoot 'em up-style game, mirroring the Entitas example.
Consider this a vertical slice; we'll cover most of apecs' features, but not in great detail.
If you are not familiar with the basics of ECS it might be worth reading the introductory sections of the paper.

If you want to run the game, clone this repository and run `stack exec shmup`.
Since this document is a literate Haskell file (or a rendered markdown file, in which case the `.lhs` file is in the same folder), you can also compile it directly with GHC and run the game.
The arrow keys move you, space shoots, escape quits.
If you have any questions or suggestions while working through this tutorial, don't hesitate to create an issue or send a message.

Let's start at the top.
Apecs' type-level machinery tends to effect a large number of pragma's.
Don't worry, GHC will happily let you know if you missed any.

> {-# LANGUAGE DataKinds                  #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE TemplateHaskell            #-}
> {-# LANGUAGE TypeApplications           #-}
> {-# LANGUAGE TypeFamilies               #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

The `Apecs` module forms the apecs prelude, it re-exports everything you typically need.

> import Apecs

For graphics and input we use `apecs-gloss`, which is a (thin) layer around the `gloss` graphics library.
Gloss is very easy to use, and ideal for simple games such as this one.

> import Apecs.Gloss

The `linear` library is the de facto library for small-dimensional vector types.

> import Linear

Finally, we use `random` for our RNG, and import some base stuff.

> import System.Random
> import System.Exit
> import Control.Monad
> import Data.Monoid
> import Data.Semigroup (Semigroup)

We need `Monoid` for `mempty`, but in recent GHC's that requires also defining `Semigroup` instances.
So, depending on your GHC version, you might not actually need the `Semigroup` import/instances.

With the imports taken care of, we can start defining Components.
We do so by first defining a data type, and then give it an instance of `Component`.

Each Component is stored in a separate data structure, called its storage or store.
The `Component` instance declaration specifies which store a Component uses.
We'll mostly be using the most basic store here, called `Map`.

`Position` and `Velocity` are straightforward Components; they define an Entity's position and velocity as two-dimensional vectors of `Float`s.
The reason we use `Float` over `Double` is that most OpenGL-based libraries, including gloss, use `Float`s.
You can use `Double`, but if you don't need the extra accuracy, using `Float` will save you a bunch of conversions.

> newtype Position = Position (V2 Float) deriving Show
> instance Component Position where type Storage Position = Map Position
> 
> newtype Velocity = Velocity (V2 Float) deriving Show
> instance Component Velocity where type Storage Velocity = Map Velocity

The following two Components are unit types, i.e. they only have a single inhabitant.
Unit types are common in apecs, as they can be used to tag an Entity.

> data Target = Target deriving Show
> instance Component Target where type Storage Target = Map Target
> 
> data Bullet = Bullet deriving Show
> instance Component Bullet where type Storage Bullet = Map Bullet

`Particle` is also used to tag an Entity, but unlike `Target` and `Bullet`, also has a remaining life span (in seconds) field.

> data Particle = Particle Float deriving Show
> instance Component Particle where type Storage Particle = Map Particle

`Player` is a unit type, but instead of storing it in a `Map`, we use a `Unique`.
A `Unique` is a `Map` that will only hold a single Component; if we assign `Player` to Entity 3 and then to Entity 4, only Entity 4 will have a `Player`.
This enforces that there is only ever one player at the store level, and it will be the first example of how a store can change the behavior of a Component.

> data Player = Player deriving Show
> instance Component Player where type Storage Player = Unique Player

The third store we will use is `Global`, used to model global variables.
`Global` stores also hold a single Component, but unlike `Unique`, that Component does not belong to any particular Entity.
Instead, a `Global` store will always yield its one Component, regardless of the Entity it is queried for.
So more accurately, a global Component belongs to /every/ Entity.

The initial value of a `Global` will be drawn from that Component's `Monoid` instance.

`Score` keeps the score, and `Time` the total elapsed time.

> newtype Score = Score Int deriving (Show, Num)
> instance Semigroup Score where (<>) = (+)
> instance Monoid Score where mempty = 0
> instance Component Score where type Storage Score = Global Score
> 
> newtype Time = Time Float deriving (Show, Num)
> instance Semigroup Time where (<>) = (+)
> instance Monoid Time where mempty = 0
> instance Component Time where type Storage Time = Global Time

You might already have noticed that there is more than one way to divide the game state into Components.
For example, since `Player`, `Target`, `Bullet`, and `Particle` are mutually exclusive, we could have enforced that by defining a single Component like this:

< data EtyType = Player | Target | Bullet | Particle Float

Defining separate Components makes it easier to efficiently iterate over one type of Entity, so that's the approach we will use in this tutorial, but both ways are equally valid and can be equally fast.

Now that we have defined our Components, we need to create a game world.
This is generally done through Template Haskell, as follows:

> makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle, ''Camera]

`makeWorld` defines a `World` data type, and the necessary instances.
More information on what exactly it generates can be found in the apecs paper.

At this point I also like to define some type synonyms and constants:

> type System' a = System World a
> type Kinetic = (Position, Velocity)
> 
> playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
> playerSpeed = 170
> bulletSpeed = 500
> enemySpeed  = 80
> xmin = -100
> xmax = 100
> 
> hitBonus, missPenalty :: Int
> hitBonus = 100
> missPenalty = 40
> 
> playerPos, scorePos :: V2 Float
> playerPos = V2 0 (-120)
> scorePos  = V2 xmin (-170)

With that, we are ready to start writing our first Systems.

> initialize :: System' ()
> initialize = do
>   playerEty <- newEntity (Player, Position playerPos, Velocity 0)
>   return ()

`initialize` initializes our game state.
In this case we only create a player, at the initial player position and with a velocity of 0.
`playerEty` is a value of type `Entity`, which is actually just an integer value.
In this case it will be 0, since it is the first Entity, and counting starts at 0.
In practice, we almost never use `Entity` values directly, and we won't actually use the `playerEty` value.

> stepPosition :: Float -> System' ()
> stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

`stepPosition` is the canonical example of a System; it adds every Entity's velocity to its position.
`cmap` is ubiquitous in apecs, and you'll use it to define most of your game logic.
`cmap`'s behaviour is heavily dependent on the type of the function we map, so I will briefly discuss that type for every use of `cmap`, and how to interpret it.
In this case, that's `(Position, Velocity) -> Position`.

`cmap` will iterate over every Entity that has the Component on the left-hand side, and write the Component on the right-hand side.
In this case, the left-hand Component is `(Position, Velocity)`.
As you can see, a tuple of Components is considered a Component as well, and it is the first example of how Components can be Composed into bigger Components.
When we iterate over a tuple, what happens internally is that we iterate over all Entities that have the first Component (`Position`), and then test whether the Entity also has the remaining Components, in this case `Velocity`.

> clampPlayer :: System' ()
> clampPlayer = cmap $ \(Player, Position (V2 x y))
>                    -> Position (V2 (min xmax . max xmin $ x) y)

`clampPlayer` constrains the player's x position between `xmin` and `xmax`.
We can express it using a simple `cmap` as well.
The function has type `(Player, Position) -> Position`, which iterates over `Player`, reads `Player` and `Position`, and writes `Position`.

Here we see the usefulness of unit types.
We never have to worry about the actual `Entity` value of the player, but instead we just refer to it using the `Player` Component.
Since `Player` is a `Unique` value, we can be sure that this only ever affects at most one Entity.

> incrTime :: Float -> System' ()
> incrTime dT = modify global $ \(Time t) -> Time (t+dT)

`incrTime` increments the total elapsed time by `dT`.
In this case, we cannot use `cmap`, as we cannot iterate over a `Global`.
If you try to do so, you will get a type error about how `Global Time` does not have an instance of `ExplMembers`; which is to say you cannot retrieve a list of members from a `Global`.
Instead we have to use `modify`, which is like `cmap` for a single Entity.
As mentioned before, the exact Entity argument does not matter for a global Component.
`global` is just an alias for -1.

Side note:
In earlier versions of apecs, the members of a global were defined to be `[ -1 ]`, so that you could `cmap` over a global.
This was removed, since it violated a number of common-sense properties such as `cmap` over `(a, b)` being semantically equivalent to `(b, a)`.
So, now it is simply a type error.

Let's make things more interesting.
`clearTargets` needs to destroy the targets that move out of bounds.
We are going to try expressing this using `cmap`.

An important thing to be aware of is that in apecs, there is no such thing as destroying/deleting/removing an /Entity/.
Instead, you can only destroy /Components/, and if you want to get rid of an Entity entirely, you often need to destroy each of its Components individually.

Our mapped function will have type `(Target, Position, Velocity) -> Maybe (Target, Position, Velocity)`.
`Maybe` represent optionality, on the left-hand side it represents a read that might fail, on the right-hand side it is a write that can also delete a Component.
If we return a `Just c`, `c` gets written as normal, but when we return `Nothing`, those same Components will be deleted instead.

> clearTargets :: System' ()
> clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
>   if x < xmin || x > xmax
>      then Nothing
>      else Just all

This works fine, but it's not ideal.
We don't really need to read `Velocity`, we are only ever interested in removing it.
Furthermore, we don't want to have to write all three Components, we just want to be able to delete them.

The next System illustrates how we can make our `cmap`s more specific.
`stepParticles` needs to decrement the life time of all `Particle`s, and remove them if their timer reaches 0.
So we need to iterate over and read `Particle`, and either delete `Particle`, `Position`, and `Velocity`, or write `Particle`.
This is done with a function of type `Particle -> Either Particle (Not (Particle, Kinetic))`.

As in most Haskell libraries, where tuples represent conjunction, `Either` represents a disjunction.
In the case of apecs, the Component `(a,b)` represents the presence of both `a` and `b`, whereas `Either a b` represents the presence of at least one of `a` or `b` (with `b` having precedence when reading).

`Not :: Not c` can be used to delete something, just like `Nothing :: Maybe c`.
It can also occur on the left-hand side, where an Entity has a Component `(a, Not b)` if it has an `a`, but no `b`.
We will see this behaviour later.

Combined, `Either a (Not b)` will either write `a`, or delete `b`.

> stepParticles :: Float -> System' ()
> stepParticles dT = cmap $ \(Particle t) ->
>   if t < 0
>      then Right $ Not @(Particle, Kinetic)
>      else Left  $ Particle (t-dT)

If you've never seen it, the `Not @c` syntax is from the `TypeApplications` pragma, and is equivalent to `Not :: Not c`.

We can take `cmap` even further.
For bullets, we want to clear them when they leave the screen, and if so, decrement the score.

We will use a function of type `(Bullet, Position, Score) -> Either () (Not (Bullet, Kinetic), Score)`.
Let's break this down.

* `(Bullet, Position, Score)` means we iterate over Entities that have all three of those Components.
  Score is a global Component, and as explained previously, can be said to belong to every Entity.
  Remember though, we cannot iterate over globals, so you get a type error if `Score` is in the first position on the left-hand side.

* Writing a `Left ()` value does nothing.

* Writing `Right (Not, Score s)` will both delete `(Bullet, Kinetic)`, and write `Score`.
  What happens when you change the type to `Either () (Not (Bullet, Kinetic, Score))`?
  Try it out, the answer might surprise you.

Putting it together:

> clearBullets :: System' ()
> clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
>   if y > 170
>      then Right $ (Not @(Bullet, Kinetic), Score (s-missPenalty))
>      else Left ()

In some cases, just because something can be expressed in `cmap`, does not mean it necessarily should.
In the example above, you could argue that we perform an unnecessary read on the Score, since we read it for every `Bullet`.
Furthermore, we jump through a lot of hoops just to be able to use `Left ()` to essentially do nothing.
In this particular case, we could have used

< cmapIf (\(Position y) -> y > 170) (\(_ :: Bullet) -> Not @ (Bullet, Position))

Sometimes, however, it might be easier to just add some side-effects.
It might not surprise you that there also is a `cmapM` and a `cmapM_`.
These pretty much work as you would expect, they take a function of type `cx -> SystemT w m cy`, meaning they iterate over and read their input argument, and run the System in the output.

We'll use `cmapM_` to do some collision handling.
We first iterate over all `(Target, Position, Entity)`s, and for each, iterate over all `(Bullet, Position, Entity)` Entities.
Then, when the distance between two positions is below the threshold, we destroy both Entities, create a bunch of particles, and update the score.

The `Entity` Component is the same `Entity` returned by e.g. `newEntity`, it is just an integer value in a newtype.
When read as a Component, it will return whatever Entity it is queried at, i.e. for `Entity ety' <- get ety`, `ety' == ety` will be true by definition.

Destroying Components for a specific Entity (rather than through `cmap`) is done with `destroy`.

Side note:
There are 5 primitive operations on stores/Components: `exists`, `get`, `set`, `destroy`, and `members`.
All other `System`s are implemented using these 5 operations.
For example, we could write `cmap` as follows:

< myCmap :: (..) => (cx -> cy) -> System' ()
< myCmap f = do
<   etys <- members (Proxy @cx)
<   forM_ etys $ \ety -> do
<     cx <- get ety
<     set ety (f cx)

I won't go into further detail here, but the take-away here is that `cmap` is pretty ordinary; the actual magic happens by choosing interesting implementations of the above 5 functions.

Anyway, collision handling:

> handleCollisions =
>   cmapM_ $ \(Target, Position posT, etyT) ->
>     cmapM_ $ \(Bullet, Position posB, etyB) ->
>       when (norm (posT - posB) < 10) $ do
>         destroy etyT (Proxy @(Target, Kinetic))
>         destroy etyB (Proxy @(Bullet, Kinetic))
>         spawnParticles 15 (Position posB) (-500,500) (200,-50)
>         modify global $ \(Score x) -> Score (x + hitBonus)

Again, every time we delete e.g. a `Bullet`, we have to remember to also delete its `Kinetic` (position and velocity).
If you forget to do so, you will have a Component with just a `Position` and a `Velocity` floating around.
It won't really interact with anything, but it will still take up memory and have its position updated in every `stepPosition`.

People have asked why there is no way to simply delete an Entity and all of its Components.
The reason apecs can't do it for you is kind of technical, but comes down to that there is no obvious way of centrally tracking what Entities have what Components.
Furthermore, adding such a System would be limiting in other ways, and additional complexity would make it harder to integrate apecs with other Systems.
Instead, in this tutorial we use type synonyms like `Kinetic` to define hierarchies of Components and easily delete many at once.
If this is an issue for you, you could write an `All` type synonym, a tuple containing all Components.
If you then `destroy ety (Proxy @All)`, you would be sure that all Components get deleted.

`triggerEvery` runs a `System` periodically.
Nothing about this is apecs-specific, except for the `get global`, which we've seen before.
If you hadn't noticed by the way, `get` doesn't need a `Proxy` because unlike e.g. `destroy` it can infer what Component to act on from its return value.

> triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
> triggerEvery dT period phase sys = do
>   Time t <- get global
>   let t' = t + phase
>       trigger = floor (t'/period) /= floor ((t'+dT)/period)
>   when trigger $ void sys

`spawnParticles` does what it says on the tin.
The random values are generated in the IO monad, so we use `liftIO`.

> spawnParticles :: Int -> Position -> (Float,Float) -> (Float,Float) -> System' ()
> spawnParticles n pos dvx dvy = replicateM_ n $ do
>   vx <- liftIO $ randomRIO dvx
>   vy <- liftIO $ randomRIO dvy
>   t  <- liftIO $ randomRIO (0.02,0.3)
>   newEntity (Particle t, pos, Velocity (V2 vx vy))

Finally, we assemble all our pieces into a single System.

> step :: Float -> System' ()
> step dT = do
>   incrTime dT
>   stepPosition dT
>   clampPlayer
>   clearTargets
>   clearBullets
>   stepParticles dT
>   handleCollisions
>   triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
>   triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

apecs-gloss provides a layer of convenience around the gloss `Graphics.Gloss.Interface.IO.Game` module.
We'll use it to make a window, render the game, and handle player input.

Let's start by looking at input handling.
We define a function that maps each possible input to a System:

> handleEvent :: Event -> System' ()
> handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
>   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)
> 
> handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) =
>   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)
> 
> handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
>   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x+playerSpeed) 0)
> 
> handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) =
>   cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x-playerSpeed) 0)
> 
> handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
>   cmapM_ $ \(Player, pos) -> do
>     newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
>     spawnParticles 7 pos (-80,80) (10,100)
>
> handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess
> 
> handleEvent _ = return ()

Next, we'll look at drawing.
This is done by constructing gloss `Picture` values.
I recommend looking at the gloss documentation to see what sort of things you can do with it.

Our drawing function will produce such a `Picture`.
The easiest way to draw multiple Entities is to use the `foldDraw` function from apecs-gloss.
It performs a `cfold` of some drawing function, and combines all results into a larger `Picture`.

> translate' :: Position -> Picture -> Picture
> translate' (Position (V2 x y)) = translate x y
> 
> triangle, diamond :: Picture
> triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
> diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]
>
> draw :: System' Picture
> draw = do
>   player  <- foldDraw $ \(Player, pos) -> translate' pos . color white  . scale 10 20 $ triangle
>   targets <- foldDraw $ \(Target, pos) -> translate' pos . color red    . scale 10 10 $ diamond
>   bullets <- foldDraw $ \(Bullet, pos) -> translate' pos . color yellow . scale 4  4  $ diamond
> 
>   particles <- foldDraw $
>     \(Particle _, Velocity (V2 vx vy), pos) ->
>         translate' pos . color orange $ Line [(0,0),(vx/10, vy/10)]
> 
>   Score s <- get global
>   let score = color white . translate' (Position scorePos) . scale 0.1 0.1 . Text $ "Score: " ++ show s
> 
>   return $ player <> targets <> bullets <> score <> particles

And with that, we can run our little game!

> main :: IO ()
> main = do
>   w <- initWorld
>   runWith w $ do
>     initialize
>     play (InWindow "Shmup" (220, 360) (10, 10)) black 60 draw handleEvent step

That concludes our tour.
Again, please let me know if you have any questions or comments, through GitHub issues/twitter/reddit.
