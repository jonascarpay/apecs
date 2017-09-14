## apecs tutorial
###An RTS-like game

In this tutorial we'll take a look at how to write a simple RTS game using apecs.
We'll be using [SDL2](https://github.com/haskell-game/sdl2) for graphics.
Don't worry if you don't know SDL2, neither do I.
We'll only be drawing single pixels to the screen, so it should be pretty easy to follow what's going on.
The final result can be found [here](https://github.com/jonascarpay/apecs/blob/master/example/RTS.hs).
You can run it with `stack build && stack exec rts`.
I will be skipping some details, so make sure to keep it handy if you want to follow along.

#### Entity Component Systems
Entity Component Systems are frameworks for game engines.
The concept is as follows:

Your game world consists of entities.
An entity is an ID and a collection of components.
Examples of components include its position, velocity, health, and 3D model.
All of the entity's state is captured by the components it holds.
The game logic is then defined in systems that operate on the game world.
This is taking the [component pattern](http://gameprogrammingpatterns.com/component.html) to the extreme, where we can arbitrarily add and remove components from entities.
An example of a system is one that looks at all entities with both a position and a velocity, and adds their velocity to their position.

What makes most ECS fast is that we can store components of the same type together.
In fact, by storing each component together with the ID of the entity it belongs to, an entity becomes implicit altogether;
an entity can be said to exist as long as there is at least one component associating itself with that entity's ID.

Once you understand this, the API is relatively straightforward.

#### Components
In our game, we want to be able to select units and order them around.
We start by defining our components.

First up is position.
A `Position` is just a two-dimensional vector of double:
When defining a data type as a component, you have to specify how the component is stored in memory.
At the root of a storage you'll generally find one of three kinds of storage; a `Map`, `Set`, or `Global`.
In this case, we can simply store the position in a `Map`.
```haskell
newtype Position = Position {getPos :: V2 Double} deriving (Show, Num)

instance Component Position where
  type Storage Position = Map Position
```

A `Target` is whatever position the entity is moving towards.
Again, the storage is a simple `Map`
```haskell
newtype Target = Target (V2 Double)

instance Component Target where
  type Storage Target = Map Target
```

We use `Selected` to tag an entity as being currently selected by the mouse.
We can designate `Selected` as being a flag by defining a Flag instance, which in turn gives us access to the `Set` storage.
```haskell
data Selected = Selected

instance Flag Selected where flag = Selected
instance Component Selected where
  type Storage Selected = Set Selected
```

Finally, we need to store some global information about the mouse.
`Dragging` indicates that we're currently performing a box-selection.
```haskell
data MouseState = Rest | Dragging (V2 Double) (V2 Double)
instance Component MouseState where
  type Storage MouseState = Global MouseState
```

We'll probably look into the `Storage` type in more detail in a future tutorial.
Using the right storage type is important when optimizing performance, but for now these will do just fine.
In fact, in this example SDL will become a bottleneck before game logic will.

#### The game world
Defining your game world is straightforward.
The only extra thing to look out for is the `EntityCounter`.
Adding an `EntityCounter` means we can use `newEntity` to add entities to our game world, which is nice.
```haskell
data World = World
  { positions     :: Storage Position
  , targets       :: Storage Target
  , selected      :: Storage Selected
  , mouseState    :: Storage MouseState
  , entityCounter :: Storage EntityCounter
  }
```
`World` simply holds the storages of each component.
Or, to be more precise, it holds immutable references to mutable storage containers for each of your components.
When actually executing the game, we produce a world in the IO monad:
```haskell
initWorld = do
  positions  <- initStore
  targets    <- initStore
  selected   <- initStore
  mouseState <- initStoreWith Rest
  counter    <- initCounter
  return $ World positions targets selected counter
```
One last thing is to make sure we can access each of these at the type level by defining instances for `Has`:
```haskell
instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Target        where getStore = System $ asks targets
instance World `Has` Selected      where getStore = System $ asks selected
instance World `Has` MouseState    where getStore = System $ asks mouseState
instance World `Has` EntityCounter where getStore = System $ asks entityCounter
```
The boilerplate ends here, you will never need to touch your `World` or the `Has` class again.
In the future, this might be automated using Template Haskell, but it's still good to at least know what's being generated.

#### Systems
Most of your code takes place in the `System` monad.
If you want to know, a `System w` is a `ReaderT w IO`, but it doesn't really matter.
All that matters is the System allows for access to the World's underlying component stores.
Just add this alias for convenience' sake:
```haskell
type System' a = System World a
```
and remember that IO looks like:
```haskell
helloWorld :: System' ()
helloWorld = liftIO $ putStrLn "Hello World!"
```

Here's a system to get you started:
```haskell
newGuy :: System' ()
newGuy = newEntity (Position (V2 0 0))
```
It makes a new guy with a position of (0,0).
Here's another:
```haskell
newGuy2 :: System' ()
newGuy = newEntity (Position (V2 0 0), Velocity (V2 0 0))
```
That's right; components can be tupled up and used as if they were a single component.

And now for something more practical:
```haskell
addUnits :: System' ()
addUnits = replicateM_ 100 $ do
    x <- liftIO$ randomRIO (0,hres)
    y <- liftIO$ randomRIO (0,vres)
    newEntity (Position (V2 x y))
```
It adds a hundred units scattered over the field.

Say you wanted to add 1 to all positions.
That would look like this:
```haskell
cmap $ \(Position p) -> Position (p+1)
```
`cmap` takes a pure function and maps it over all components in the domain of the function.

`cmap'` is analogous, but takes a function of `c -> Safe c`.
A `Safe` value comes up when performing a read that might fail, or a write that might delete.
At runtime, it looks like e.g. `Safe (Just (Position p), Nothing) :: Safe (Position, Target)` when reading an entity that has a position but no target.
In the case of `cmap'`, it means that the function might delete the component it's mapped over.

There's also `rmap`, of type `(r -> w) -> System world ()`.
It still iterates over the components in the domain, but instead of mapping to those same components, it writes the result to a different component (creating one if none exists).
This can be used to write something like `rmap $ \(Position p, Velocity v) -> Position (p+v)` to step positions, or `rmap $ \ Player -> Selected` to add the `Selected` tag to the player.

Finally, there's these mapping functions, whose effect you can see from the type signature:
```haskell
rmap' :: (r -> Safe w) -> System world ()
wmap  :: (Safe r -> w) -> System world ()
wmap' :: (Safe r -> Safe w) -> System world ()
```
Note that `wmap` has a `Safe` argument in its function.
`wmap` iterates over the entities/components in the codomain of its function.
Those entities are not guaranteed to have an `r` component, so we need `Safe` here.

Let's write the first part of our game loop.
We will use `cmap'` to delete a target once we are sufficiently close:
```haskell
step = do
  let speed = 5
      stepPosition :: (Target, Position) -> Safe (Target, Position)
      stepPosition (Target t, Position p)
        | V.vlength (p-t) < speed = Safe (Nothing, Just (Position t))
        | otherwise               = Safe (Just (Target t), Just (Position (p + V.setLength speed (t-p))))

  cmap' stepPosition
```
There's a lot there.
First try to understand what `stepPosition`'s type signature means, then what the body means, and then what it means to `cmap'` that function.
Once an entity loses its `Target` component, it will no longer be affected by the function above, because it's no longer in the domain of `stepPosition`.

This is the second part of the game loop:
```haskell
  m :: MouseState <- readGlobal
  case m of
    Rest -> return ()
    Dragging (V2 ax ay) (V2 bx by) -> do
      resetStore (Proxy :: Proxy Selected)
      let f :: Position -> Safe Selected
          f (Position (V2 x y)) = Safe (x >= min ax bx && x <= max ax bx && y >= min ay by && y <= max ay by)
      rmap' f
```
We start by reading the `MouseState` global.
The result of `readGlobal` is determined by the type it is instantiated with.
After resetting the store, we determine what units are selected.
We can do this using `rmap'`. `f` looks at every `Position`, and returns `Safe True` if the position was inside the selection box.
Because `Selected` is a `Set`, its `Safe` representation is a `Bool` rather than `Maybe c`.

### Events
```haskell
handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (P p))) =
  let p' = fromIntegral <$> p in writeGlobal (Dragging p' p')

handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _)) =
  writeGlobal Rest

handleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (P p) _)) = do
  md <- readGlobal
  case md of
    Rest -> return ()
    Dragging a _ -> writeGlobal (Dragging a (fromIntegral <$> p))

handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonRight _ (P (V2 px py)))) = do
  sl :: Slice Selected <- slice All
  let r = (*3) . subtract 1 . sqrt . fromIntegral$ sliceSize sl

  sliceForM_ sl $ \e -> do
    dx <- liftIO$ randomRIO (-r,r)
    dy <- liftIO$ randomRIO (-r,r)
    set (cast e) (Target (V2 (fromIntegral px+dx) (fromIntegral py+dy)))

handleEvent _ = return ()
```
