This document breaks down a full game written using apecs.
We're going to be making a little shoot 'em up-style game, mirroring the Entitas example.
We'll cover most of apecs features, but not in great detail.
If you are not familiar with the basics of ECS, I recommend reading at least the introductory sections of the paper.

Since this document is a literate Haskell file (unless you're reading the markdown version), you can compile it with GHC and run the game.
I recommend doing so before reading, so you know what we're making.
Make sure to also include the `linear`, `gloss`, and `random` libraries.
The arrow keys move you, space shoots.

Let's start at the top.
Apecs tends to effect a large number of pragma's, as you can see below.
GHC will let you know if you missed any.
Some of these, like TypeApplications, are not strictly necessary, but provide some nice syntactic sugar.

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

For graphics and input we use gloss.
Gloss is really easy to use for simple games such as this one, but it's not very suited for larger projects.

> import           Graphics.Gloss
> import           Graphics.Gloss.Interface.IO.Game

The linear library is the de facto library for small-dimensional vector types, V2 in our case.

> import           Linear

Finally, we use random for our RNG, and import some base stuff.

> import           System.Random
> import           Control.Monad
> import           Data.Monoid

With the imports taken care of, we can start defining components.
We do so by first defining a data type for our component, and then give it an instance of `Component`.
The instance specifies the data structure we store a component in, in the following cases that will be `Map c`.
Stores change both the behaviour and performance characteristics of your code.
We will only look at stores that change the behaviour here.

`Position` and `Velocity` are straightforward; they define an entity's position and velocity as two-dimensional vectors of `Double`s.

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

`Player` is unit type, but instead of storing it in a `Map`, we use a `Unique`.
A `Unique` is a `Map` that will only hold a single component; if we assign `Player` to entity 3 and then to entity 4, only entity 4 will have a `Player`.
This enforces that there is only ever one player at the store level, and the first example of how a store can change the behavior of a component.

> data Player = Player deriving Show
> instance Component Player where type Storage Player = Unique Player

The third store we will use is `Global`, used to model global variables.
`Global` stores also hold a single component, but unlike `Unique`, that component does not belong to any particular entity.
Instead, a `Global` store will always yield its component, regardless of the entity it is queried for.
This means that we can use both `get 0` and `get 123` to retrieve a global component, but usually we just do `get global`.
The initial value of a global will be drawn from that component's Monoid instance, as this is the most unambiguous way of defining a default value in Haskell.

`Score` keeps the score, and `Time` the total elapsed time.

> newtype Score = Score Int deriving Show
> instance Semigroup Score
> instance Monoid Score where mempty = Score 0
> instance Component Score where type Storage Score = Global Score
> 
> newtype Time = Time Double deriving Show
> instance Semigroup Time
> instance Monoid Time where mempty = Time 0
> instance Component Time where type Storage Time = Global Time

You might already have noticed that there is more than one way to divide your game state into components.
For example, since `Player`, `Target`, `Bullet`, and `Particle` are mutually exclusive, we could have enforced that by doing

< data EtyType = Player | Target | Bullet | Particle Color Double

There are many reasons you might want to use one over the other, and a full discussion would be way too long to include here.
In this case, I chose to split as it makes it easier to iterate over e.g. just the `Target`s.
Just know that there is nothing wrong with the other approach, however, and with some effort, we can make it just as fast.

Now that we have defined our components, we need to create a game world.
This is generally done through Template Haskell, as follows:

> makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle]

To understand apecs it is important to understand what `makeWorld` expands to, which is detailed in the paper.

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

`initialize` will initialize our game state.
In this case we only need to create a player, at the initial player position and with a velocity of 0:

> initialize :: System' ()
> initialize = void $ newEntity (Player, Position playerPos, Velocity 0)

`stepPosition` is the canonical example of a system; it adds every entity's velocity to its position.
`cmap`'s behaviour is heavily dependent on the type of the function we map, so I will briefly discuss that type for every use of `cmap`.
In this case, that's `(Position, Velocity) -> Position`, which means we iterate using `Position`, we read `Position` and `Velocity`, and we write `Position`.

> stepPosition :: Double -> System' ()
> stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

`clampPlayer` constrains the player's x position between xmin and xmax, and we can also express it using a simple `cmap`.
The function has type `(Player, Position) -> Position`, which iterates over `Player`, reads `Player` and `Position`, and writes `Position`.

> clampPlayer :: System' ()
> clampPlayer = cmap $ \(Player, Position (V2 x y))
>                    -> Position (V2 (min xmax . max xmin $ x) y)

`incrTime` increments the total elapsed time by `dT`.
In this case, we cannot use `cmap`, as we cannot lawfully iterate over a `Global`.
If you try to do so, you will get a type error about how `Global Time` does not have an instance of `ExplMembers`; you cannot retrieve a list of members from a `Global`.
Instead we have to use `modify`, which is like `cmap` for a specific entity.
As mentioned before, the exact entity argument does not matter for a global component.
We use 0 in this case for demonstration purposes, but after this we will use `global`.

> incrTime :: Double -> System' ()
> incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

Let's make things more interesting.
`clearTargets` needs to destroy the targets that move out of bounds.
We can also express this using `cmap`.
In general, if you only want to delete a component some of the time, you use a `Maybe`.
`Maybe` represent optionality, it can do a read that might fail if the component is not present, or it can do a write that can optionally delete.
In this case, the function has type `(Target, Position, Velocity) -> Maybe (Target, Position, Velocity)`.
It iterates over `Target`, reads `Target`, `Position`, and `Velocity`, and either writes or deletes `Target`, `Position`, and `Velocity`.

> clearTargets :: System' ()
> clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
>   if x < xmin || x > xmax
>      then Nothing
>      else Just all

Unfortunately, this definition is not ideal.
We don't really need to read `Velocity`, we are only interested in removing it.
Furthermore, we don't want to write all three components, we just want to be able to delete them.

The next System illustrates how we can solve issues such as these, with some more `cmap` magic.
`stepParticles` needs to decrement the life time of all the particles, and remove them if it reaches 0.
So we need to iterate over and read `Particle`, and either delete `Particle`, `Position`, and `Velocity`, or write `Particle`.
This can be done with a function of type `Particle -> Either Particle (Not (Particle, Position, Velocity))`.
`Not :: Not c` can be used to delete something, just like `Nothing :: Maybe c`.
`Either` can be used to write one of two things.
Combined, `Either a (Not b)` will either write `a`, or delete `b`.

`Either` can also occur in the input of the function, e.g. `Either a b -> c`, which will first attempt to read `b`, or read `a` if that fails.
The order is done this way to stick to the interpretation of `Either error result`.
By the way, if you've never seen it, the `Not @c` is from the TypeApplication pragma, and is equivalent to `Not :: Not c`.

> stepParticles :: Double -> System' ()
> stepParticles dT = cmap $ \(Particle col t) ->
>   if t < 0
>      then Right $ Not @(Particle, Kinetic)
>      else Left  $ Particle col (t-dT)

We can take this concept even further.
For bullets, we want to clear them when they leave the screen, and if so, decrement the score.
We use a function of type `(Bullet, Position, Score) -> Either () (Not (Bullet, Kinetic), Score)`.
This will iterate over `Bullet`, read `Bullet`, `Position` and `Score`, and either write `()`, which does nothing, or it will delete `Bullet` and `Kinetic`, and write `Score`.

> clearBullets :: System' ()
> clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
>   if y > 170
>      then Right $ (Not @(Bullet, Kinetic), Score (s-missPenalty))
>      else Left ()

In some cases, just because something can be expressed in `cmap`, does not mean it necessarily should.
In the example above, you could argue that we perform an unnecessary read on the Score, since we read it for every `Bullet`.
You might instead want to perform the update of the `Score` as a side-effect instead.
It then might not surprise you that there also is a `cmapM` and a `cmapM_`.
These pretty much work as you would expect, they iterate over and read their input argument, and run the System in the output.
Here, we use it to do collision handling.

We first iterate over all `Targets`, and read their `Positions` and `Entity` index.
We then iterate over all `Bullets`, and also read `Position` and `Entity`.
Then, when the distance betweeen the two positions is below the threshold, we destroy both entities, create a bunch of particles, and update the score.
You can also see how we use `global` as the argument to `modify` instead of 0, to make it clear that this is a global variable.

> handleCollisions =
>   cmapM_ $ \(Target, Position posT, etyT) ->
>     cmapM_ $ \(Bullet, Position posB, etyB) ->
>       when (norm (posT - posB) < 10) $ do
>         destroy etyT (Proxy @(Target, Kinetic))
>         destroy etyB (Proxy @(Bullet, Kinetic))
>         spawnParticles 15 (Position posB) white (-500,500) (200,-50)
>         modify global $ \(Score x) -> Score (x + hitBonus)

Let's briefly talk about deleting components.
As you might have noticed, every time we delete e.g. a `Bullet`, we have to remember to also delete its `Kinetic` (position and velocity).
If you forget to do so, you will have a component with just a `Position` and a `Velocity` floating around.
It won't really interact with anything, but it will still take up memory and have its position updated in every `stepPosition`.

In other words, it is a potential memory leak, and I've been asked how to deal with that, or why apecs can't just delete every component for some entity.
After all, in OO languages, deleting an object naturally also deletes all its components.
The reason apecs can't do it for you is kind of technical, but comes down to that there is no obvious way of centrally tracking what entities have what components (but if you have any ideas, let me know).
This might change in the future, but it does not seem like the additional effort of implementing such a system is even really worth it, since there are fairly simple ways of dealing with this yourself if you think this might be a problem.

First of all, you could define a type synonym like `type All = (Enemy, Bullet, Player, Particle, Kinetic)`, and simply destroy `@All` every time you want to delete an entity.
Once you have a lot of components, however, this starts performing a lot of unnecessary deletions, and if you forget to include some component, you still have a leak.

A more scalable way is through proper use of type synonyms, which should help your code in general.
We have already seen how `Kinetic` allows us to abbreviate `Position` and `Velocity`.
In larger games this might extend to something like

< type Material = (Density, Elasticity)
< type Physics  = (Kinetic, Model, Material)
< type UnitType = Either Player EnemyType
< type Unit = (UnitType, Physics)

which then allows you to do destroy `@Unit`.
Defining composite components such as these is /really/ useful for larger games, not just for deleting.
For instance, we can now check if any entity has a `Position` and `Velocity` but is not a `Unit`:

< findIncompleteEntities = cmapM_ $
<   \(_ :: Kinetic, Not :: Not Unit, ety :: Entity)
<     -> liftIO (print ety)

Finally, you could define a store for multiple components, that will delete all components for some entity whenever a single one is deleted.
This happens in apecs-physics, for example, where deleting a `Body` will automatically delete all attached shapes and physical properties.
Once your game scales up it becomes useful to know how to define custom stores, as you will likely want stores with additional features like spatial hashing anyway.

To conclude, be wary of the fact that you will have to properly clean up components.
As long as you define useful type synonyms, your life will improve in more ways than just easier deletion of components.

Back to the actual game code.
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
