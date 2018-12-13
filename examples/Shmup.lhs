This document breaks down a small game written using apecs.
We're going to be making a little shoot 'em up-style game, mirroring the Entitas example.
We'll cover most of apecs' features, but not in great detail.
If you are not familiar with the basics of ECS it might be worth reading the introductory sections of the paper.

If you want to run the game, clone this repository and run `stack exec shmup`.
Since this document is a literate Haskell file (or a rendered markdown file, in which case the `.lhs` file is in the same folder), you can also compile it directly with GHC and run the game.
The arrow keys move you, space shoots.

Let's start at the top.
Apecs tends to effect a large number of pragma's, as you can see below.
GHC will let you know if you missed any.

> {-# LANGUAGE DataKinds             #-}
> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables   #-}
> {-# LANGUAGE TemplateHaskell       #-}
> {-# LANGUAGE TypeApplications      #-}
> {-# LANGUAGE TypeFamilies          #-}

The `Apecs` module forms the apecs prelude, it re-exports everything you typically need.

> import           Apecs

For graphics and input we use `apecs-gloss`, which is a (thin) layer around the `gloss` graphics library.
Gloss is very easy to use, and ideal for simple games such as this one.

> import           Apecs.Gloss

The `linear` library is the de facto library for small-dimensional vector types.

> import           Linear

Finally, we use `random` for our RNG, and import some base stuff.

> import           System.Random
> import           Control.Monad
> import           Data.Monoid
> import           Data.Semigroup (Semigroup)

With the imports taken care of, we can start defining components.
We do so by first defining a data type, and then give it an instance of `Component`.

Each `Component` is stored in a separate data structure, called its storage or store.
The instance declaration specifies which store a component uses.
In our case, we'll mostly be using the most basic store, called `Map`.

`Position` and `Velocity` are straightforward `Components`; they define an entity's position and velocity as two-dimensional vectors of `Double`s.

> newtype Position = Position (V2 Double) deriving Show
> instance Component Position where type Storage Position = Map Position
> 
> newtype Velocity = Velocity (V2 Double) deriving Show
> instance Component Velocity where type Storage Velocity = Map Velocity

The following two components are unit types, i.e. they only have a single inhabitant.
Unit types are common in apecs, as they can be used to tag an entity.

> data Target = Target deriving Show
> instance Component Target where type Storage Target = Map Target
> 
> data Bullet = Bullet deriving Show
> instance Component Bullet where type Storage Bullet = Map Bullet

`Particle` is also used to tag an entity, but unlike `Target` and `Bullet`, also includes a color and a remaining life span (in seconds).

> data Particle = Particle Color Double deriving Show
> instance Component Particle where type Storage Particle = Map Particle

`Player` is a unit type, but instead of storing it in a `Map`, we use a `Unique`.
A `Unique` is a `Map` that will only hold a single component; if we assign `Player` to entity 3 and then to entity 4, only entity 4 will have a `Player`.
This enforces that there is only ever one player at the store level, and the first example of how a store can change the behavior of a component.

> data Player = Player deriving Show
> instance Component Player where type Storage Player = Unique Player

The third store we will use is `Global`, used to model global variables.
`Global` stores also hold a single component, but unlike `Unique`, that component does not belong to any particular entity.
Instead, a `Global` store will always yield its one component, regardless of the entity it is queried for.
So more accurately, a global component belongs to /every/ entity.

The initial value of a `Global` will be drawn from that component's `Monoid` instance.

`Score` keeps the score, and `Time` the total elapsed time.

> newtype Score = Score Int deriving Show
> instance Semigroup Score -- Required in GHC 8.4
> instance Monoid Score where mempty = Score 0
> instance Component Score where type Storage Score = Global Score
> 
> newtype Time = Time Double deriving Show
> instance Semigroup Time -- Required in GHC 8.4
> instance Monoid Time where mempty = Time 0
> instance Component Time where type Storage Time = Global Time

You might already have noticed that there is more than one way to divide the game state into components.
For example, since `Player`, `Target`, `Bullet`, and `Particle` are mutually exclusive, we could have enforced that by defining a single component like this:

< data EtyType = Player | Target | Bullet | Particle Color Double

Defining separate components makes it easier to efficiently iterate over one type of entity, so that's the approach we will use in this tutorial, but both ways are equally valid and can be equally fast.

Now that we have defined our components, we need to create a game world.
This is generally done through Template Haskell, as follows:

> makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle]

`makeWorld` defines a `World` data type, and the necessary instances for the type-level machinery.
More information can be found in the apecs paper.

At this point I also like to define some type synonyms and constants:

> type System' a = System World a
> type Kinetic = (Position, Velocity)
> 
> playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Double
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
> playerPos, scorePos :: V2 Double
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
In this case, it will be 0, since it is the first entity.
In practice, we almost never use `Entity` values directly, and we won't actually use the `playerEty` value.

> stepPosition :: Double -> System' ()
> stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

`stepPosition` is the canonical example of a system; it adds every entity's velocity to its position.
`cmap` is the most important operation in apecs, and you'll use it to define most of your game logic.
`cmap`'s behaviour is heavily dependent on the type of the function we map, so I will briefly discuss that type for every use of `cmap`, and how to interpret it.
In this case, that's `(Position, Velocity) -> Position`.

`cmap` will iterate over every entity that has the component on the left-hand side, and write the components on the right-hand side.
In this case, the left-hand component is `(Position, Velocity)`.
As you can see, a tuple of components is considered a component as well, and it is the first example of how components can be composed into bigger components.
When we iterate over a tuple, what happens internally is that we iterate over the first component (`Position`), and then test whether the entities we iterate over also have the remaining components, in this case `Velocity`.

> clampPlayer :: System' ()
> clampPlayer = cmap $ \(Player, Position (V2 x y))
>                    -> Position (V2 (min xmax . max xmin $ x) y)

`clampPlayer` constrains the player's x position between `xmin` and `xmax`, and we can also express it using a simple `cmap`.
The function has type `(Player, Position) -> Position`, which iterates over `Player`, reads `Player` and `Position`, and writes `Position`.
Here we see an example of the usefulness of unit types.
We never have to worry about the actual `Entity` value of the player, but instead we just refer to it using the `Player` type.
Since `Player` is a `Unique` value, we can be sure that this only ever affects at most one entity.

> incrTime :: Double -> System' ()
> incrTime dT = modify global $ \(Time t) -> Time (t+dT)

`incrTime` increments the total elapsed time by `dT`.
In this case, we cannot use `cmap`, as we cannot iterate over a `Global`.
If you try to do so, you will get a type error about how `Global Time` does not have an instance of `ExplMembers`; you cannot retrieve a list of members from a `Global`.
Instead we have to use `modify`, which is like `cmap` for a single entity.
As mentioned before, the exact entity argument does not matter for a global component.
`global` is just an alias for -1.

Side note:
In earlier versions of apecs, the members of a global were defined to be `[ -1 ]`, so that you could `cmap` over a global.
This was removed, after the store laws were formulated.
For example, there is no feasible global implementation that preserves the property that `cmap` over `(a, b)` is semantically equivalent to `(b, a)`.
So, now it is simply a type error.

Let's make things more interesting.
`clearTargets` needs to destroy the targets that move out of bounds.
We are going to try expressing this using `cmap`.

An important thing to be aware of is that in apecs, there is no such thing as destroying an entity.
Instead, you always have to destroy each individual component.

Our mapped function will have type `(Target, Position, Velocity) -> Maybe (Target, Position, Velocity)`.
`Maybe` represent optionality, on the left-hand side it represents a read that might fail, on the right-hand side it is a write that can also delete a component.
If we return a `Just c`, `c` gets written as normal, but when we return `Nothing`, those same components will be deleted instead.

> clearTargets :: System' ()
> clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
>   if x < xmin || x > xmax
>      then Nothing
>      else Just all

Unfortunately, this definition is not ideal.
We don't really need to read `Velocity`, we are only interested in removing it.
Furthermore, we don't want to write all three components, we just want to be able to delete them.

The next System illustrates how we can make our `cmap`s more specific.
`stepParticles` needs to decrement the life time of all `Particle`s, and remove them if their timer reaches 0.
So we need to iterate over and read `Particle`, and either delete `Particle`, `Position`, and `Velocity`, or write `Particle`.
This is done with a function of type `Particle -> Either Particle (Not (Particle, Kinetic))`.

As in most Haskell libraries, where tuples represent conjunction, `Either` represents a disjunction.
In the case of apecs, the component `(a,b)` represents the presence of both `a` and `b`, whereas `Either a b` represents the presence of at least one of `a` or `b` (with `b` having precedence when reading).

`Not :: Not c` can be used to delete something, just like `Nothing :: Maybe c`.

Combined, `Either a (Not b)` will either write `a`, or delete `b`.

> stepParticles :: Double -> System' ()
> stepParticles dT = cmap $ \(Particle col t) ->
>   if t < 0
>      then Right $ Not @(Particle, Kinetic)
>      else Left  $ Particle col (t-dT)

If you've never seen it, `Not @c` is from the `TypeApplications` pragma, and is equivalent to `Not :: Not c`.

We can take `cmap` even further.
For bullets, we want to clear them when they leave the screen, and if so, decrement the score.

We will use a function of type `(Bullet, Position, Score) -> Either () (Not (Bullet, Kinetic), Score)`.
Let's break this down.

* `(Bullet, Position, Score)` means we iterate over entities that have all three of those components.
  Score is a global component, and as explained previously, can be said to belong to every entity.
  Remember though, we cannot iterate over globals, so you get a type error if `Score` is in the first position on the left-hand side.

* Writing a `Left ()` value does nothing.

* Writing `Right (Not, Score s)` will both delete `(Bullet, Kinetic)`, and write `Score`.
  What happens when you change the type to `Either () (Not (Bullet, Kinetic, Score))`?
  The answer might surprise you~

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
Then, when the distance between two positions is below the threshold, we destroy both entities, create a bunch of particles, and update the score.

The `Entity` component is the same `Entity` returned by e.g. `newEntity`, it is just an integer value in a newtype.
When read as a component, it will return whatever Entity it is queried at, i.e. for `Entity ety' <- get ety`, `ety' == ety` will be true by definition.

Destroying components for a specific Entity (rather than through `cmap`) is done with `destroy`.

Side note:
Every store, and by extension every component, has 5 primitive operations: `exists`, `get`, `set`, `destroy`, and `members`.
All other `System`s are implemented using these 5 operations.
For example, we could write `cmap` as follows:

< cmap :: (..) => (cx -> cy) -> System' ()
< cmap f = do
<   etys <- members (Proxy @cx) -- members needs a Proxy to infer its components
<   forM_ etys $ \ety -> do
<     cx <- get ety
<     set ety (f cx)

I won't go into further detail here, but the message here is that `cmap` is pretty ordinary; the magic happens by choosing interesting implementations of the above 5 functions.

Anyway, collision handling:

> handleCollisions =
>   cmapM_ $ \(Target, Position posT, etyT) ->
>     cmapM_ $ \(Bullet, Position posB, etyB) ->
>       when (norm (posT - posB) < 10) $ do
>         destroy etyT (Proxy @(Target, Kinetic))
>         destroy etyB (Proxy @(Bullet, Kinetic))
>         spawnParticles 15 (Position posB) white (-500,500) (200,-50)
>         modify global $ \(Score x) -> Score (x + hitBonus)

Again, every time we delete e.g. a `Bullet`, we have to remember to also delete its `Kinetic` (position and velocity).
If you forget to do so, you will have a component with just a `Position` and a `Velocity` floating around.
It won't really interact with anything, but it will still take up memory and have its position updated in every `stepPosition`.

People have asked why there is no way to simply delete an Entity and all of its components.
The reason apecs can't do it for you is kind of technical, but comes down to that there is no obvious way of centrally tracking what entities have what components.
Furthermore, adding such a system would be limiting in other ways.
Instead, in this tutorial we use type synonyms like `Kinetic` to define hierarchies of components and easily delete many at once.

`triggerEvery` runs a `System` periodically.
It uses `get` to read the `Time`, again using `global`.

> triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
> triggerEvery dT period phase sys = do
>   Time t <- get global
>   let t' = t + phase
>       trigger = floor (t'/period) /= floor ((t'+dT)/period)
>   when trigger $ void sys

`spawnParticles` does what it says on the tin.
The random values are generated in the IO monad, so we use `liftIO`.

> spawnParticles :: Int -> Position -> Color -> (Double,Double) -> (Double,Double) -> System' ()
> spawnParticles n pos color dvx dvy = replicateM_ n $ do
>   vx <- liftIO $ randomRIO dvx
>   vy <- liftIO $ randomRIO dvy
>   t  <- liftIO $ randomRIO (0.02,0.3)
>   newEntity (Particle color t, pos, Velocity (V2 vx vy))

Finally, we assemble all our pieces into a single system.

> step :: Double -> System' ()
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

Next, handling player input.
Gloss makes this really easy, we just need to map `Event` values to Systems:

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
>     spawnParticles 7 pos yellow (-80,80) (10,100)
> 
> handleEvent _ = return ()

Rendering in gloss means producing a `Picture` value (and a lot of Double/Float conversion).
Since pictures are composed monoidically, we can do this in `cfold`.
We have not seen `cfold` before, but it is to `cmap` as `foldl` is to `map`.
`drawComponents` takes a drawing function for a single component, and uses it to draw every such component:

> drawComponents :: Get World IO c => (c -> Picture) -> System' Picture
> drawComponents f = cfold
>   (\pic (Position p, c) -> pic <> translate' p (f c))
>   mempty
> 
> translate' :: V2 Double -> Picture -> Picture
> translate' (V2 x y) = translate (realToFrac x) (realToFrac y)

We then define some primitives, and assemble them into a full picture.
Most of the code here is gloss-related, so I won't go into a lot of detail.

> triangle, diamond :: Picture
> triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
> diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]
> 
> draw :: System' Picture
> draw = do
>   player  <- drawComponents $ \Player -> color white  . scale 10 20 $ triangle
>   targets <- drawComponents $ \Target -> color red    . scale 10 10 $ diamond
>   bullets <- drawComponents $ \Bullet -> color yellow . scale 4  4  $ diamond
> 
>   particles <- drawComponents $
>     \(Particle col _, Velocity (V2 vx vy))
>     -> color col $ Line [(0,0),(realToFrac vx/10, realToFrac vy/10)]
> 
>   Score s <- get global
>   let score = color white . translate' scorePos . scale 0.1 0.1 . Text $ "Score: " ++ show s
> 
>   return $ player <> targets <> bullets <> score <> particles
>   where

You run a game in gloss using the `playIO` function.
We need to do some marshalling between System and IO to line up the types.

> playGloss :: w
>           -> System w Picture
>           -> (Event -> System w ())
>           -> (Double -> System w ())
>           -> IO ()
> playGloss world drawSys eventSys stepSys =
>   playIO
>     window black fps ()
>     (\_    -> runSystem drawSys world)
>     (\e _  -> runSystem (eventSys e) world)
>     (\dt _ -> runSystem (stepSys $ realToFrac dt) world)
>   where
>     window = InWindow "game" (220,360) (10,10)
>     fps = 60

Finally, we run the game!

> main :: IO ()
> main = do
>   w <- initWorld
>   runSystem initialize w
>   playGloss w draw handleEvent step

That concludes our tour.
If you want more information, I recommend reading the paper and haddocks.
If you have any questions, feel free to create an Issue, or ask me on twitter/reddit.
