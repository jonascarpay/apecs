## apecs tutorial
### An RTS-like game

In this tutorial we'll take a look at how to write a simple RTS-like game using apecs.
We'll be using [SDL2](https://github.com/haskell-game/sdl2) for graphics.
Don't worry if you don't know SDL2, neither do I.
We'll only be drawing single pixels to the screen, so it should be pretty easy to follow what's going on.
The final result can be found [here](https://github.com/jonascarpay/apecs/blob/master/examples/RTS.hs).
You can run it with `stack build && stack exec rts`.
I will be skipping some details, so make sure to keep it handy if you want to follow along.

#### Entity Component Systems
Entity Component Systems are frameworks for game engines.
The concept is as follows:

Your game world consists of entities.
An entity is an ID and a collection of components.
Examples of components include position, velocity, health, and 3D model.
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
A `Position` is just a two-dimensional vector of `Double`s.
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

Different `Storage` types have different performance characteristics, but in general, these will do just fine.
In fact, in this example SDL will become a bottleneck before game logic will.
For more information, check out [this performance guide](https://github.com/jonascarpay/apecs/blob/master/tutorials/GoingFast.md) and the [Stores module documentation](https://hackage.haskell.org/package/apecs-0.2.4.3/docs/Apecs-Stores.html).

#### The game world
Defining your game world is straightforward.
This is generally automated with `makeWorld`, but it's useful to know what's being generated.

`World` holds the stores for each component.
Or, to be more precise, it holds immutable references to mutable storage containers for each of your components.

Adding an `EntityCounter` component allows us to use `newEntity` to add entities to our game world, which is nice.
```haskell
data World = World
  { positions     :: Storage Position
  , targets       :: Storage Target
  , selected      :: Storage Selected
  , mouseState    :: Storage MouseState
  , entityCounter :: Storage EntityCounter
  }
```
We then make sure we can access each of these at the type level by defining instances for `Has`, using `asks` from `ReaderT`:
```haskell
instance World `Has` Position      where getStore = System $ asks positions
instance World `Has` Target        where getStore = System $ asks targets
instance World `Has` Selected      where getStore = System $ asks selected
instance World `Has` MouseState    where getStore = System $ asks mouseState
instance World `Has` EntityCounter where getStore = System $ asks entityCounter
```
When actually executing the game, we produce a world in the IO monad like this:
```haskell
initWorld = do
  positions  <- initStore -- initStore = initStoreWith (), used to initialize most stores
  targets    <- initStore
  selected   <- initStore
  mouseState <- initStore -- A global needs to be initialized with a value
  counter    <- initStore
  return $ World positions targets selected counter
```


#### Systems
Most of your code takes place in the `System` monad.
If you want to know, a `System w a` is a newtype for `ReaderT w IO a`, but it doesn't really matter if you don't know what that means.
All that matters is that a `System world` allows for access to the `world`'s underlying component stores.
After defining the world, I like to add this alias for convenience' sake:
```haskell
type System' a = System World a
```

Here's a system to get you started:
```haskell
helloWorld :: System' ()
helloWorld = liftIO $ putStrLn "Hello World!"
```
`liftIO` is also used to make render calls. Here's another system:
```haskell
newGuy :: System' ()
newGuy = newEntity (Position (V2 0 0))
```
It makes a new guy with a position of (0,0).
Here's another:
```haskell
newGuy2 :: System' ()
newGuy2 = newEntity (Player, Position (V2 0 0), Velocity (V2 0 0))
```
As you can see, components can be tupled up and used as if they were a single component.

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
`cmap :: (c -> c) -> System world ()` takes a pure function and maps it over all components in the domain of the function.

`cmap'` is analogous, but takes a function of type `c -> Safe c`.
A `Safe` value comes up when performing a read that might fail, or a write that might delete.
At runtime, it looks like e.g. `Safe (Just (Position p), Nothing) :: Safe (Position, Target)` when reading an entity that has a position but no target.
In the case of `cmap'`, it means that the function might delete the component it's mapped over.

Note that while the lefthand side of `::` has `Just` and `Nothing`, there is no `Maybe` on the righthand side.
This is because the `Safe` representation is determined by the `Store`'s `SafeRW` type.
For a `Map c`, that's `Maybe c`, but a `Set c`, for instance, has `Bool`.
Don't worry, if you mess up, GHC will happily and verbosely let you know where and how.

Continuing with the mapping functions, we also have `rmap`, of type `(r -> w) -> System world ()`.
It still iterates over the components in the domain, but instead of mapping to those same components, it writes the result to a different component (creating one if none exists).
This can be used to write something like `rmap $ \(Position p, Velocity v) -> Position (p+v)` to step positions, or `rmap $ \ Player -> Selected` to add the `Selected` tag to the player.
Note that `rmap` is a more general version of `cmap`, and you are free to use it wherever you could have used `cmap`.

These are the rest of the mapping functions, whose effect you can infer from their type signature:
```haskell
rmap' :: (r -> Safe w) -> System world ()
wmap  :: (Safe r -> w) -> System world ()
wmap' :: (Safe r -> Safe w) -> System world ()
```
Note that `wmap` has a `Safe` _argument_ in its function.
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
        | otherwise               = Safe (Just (Target t), Just (Position (p + speed * normalize (t-p))))

  cmap' stepPosition
```
There's a lot there.
First try to understand what `stepPosition`'s type signature means, then what the body means, and then what it means to `cmap'` that function.
It performs a step of size `speed` in the direction of `Target`, until it reaches its target at which point the `Target` component is deleted. 
Once an entity loses its `Target` component, it will no longer be affected by the function above, because it's no longer in the domain of `stepPosition`.

This is the second part of the game loop:
```haskell
  m :: MouseState <- getGlobal
  case m of
    Rest -> return ()
    Dragging (V2 ax ay) (V2 bx by) -> do
      resetStore (Proxy :: Proxy Selected)
      let f :: Position -> Safe Selected
          f (Position (V2 x y)) = Safe (x >= min ax bx && x <= max ax bx && y >= min ay by && y <= max ay by)
      rmap' f
```
We start by reading the `MouseState` global.
The result of `getGlobal` is determined by the type it is instantiated with.
`resetStore` is semantically equivalent to `cmap' $ \(_ :: Selected) -> Safe False`, i.e. it just deletes every component of some type, but more general and usually faster.
Because `Selected` is a `Set`, its `Safe` representation is a `Bool` rather than `Maybe c`.
For components in a `Map`, the equivalent of `resetStore` is `cmap' $ \(_ :: c) -> Nothing`.
After resetting the store, we determine what units are selected.
We can do this using `rmap'`.
`f` looks at every `Position`, and returns `Safe True` if the position was inside the selection box.

### Events
Handling events is unpacking SDL `Event`s and matching them to a piece of game logic:

Here we start tracking the mouse when the left button is pressed, and stop when it is released.
```haskell
handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonLeft _ (P p))) =
  let p' = fromIntegral <$> p in setGlobal (Dragging p' p')

handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Released _ SDL.ButtonLeft _ _)) =
  setGlobal Rest
```

This is how we update the selection box when the mouse moves:
```haskell
handleEvent (SDL.MouseMotionEvent (SDL.MouseMotionEventData _ _ _ (P p) _)) = do
  md <- getGlobal
  case md of
    Rest -> return ()
    Dragging a _ -> setGlobal (Dragging a (fromIntegral <$> p))
```

And finally, what to do when the right mouse button is pressed.
As per genre convention, the selected units are to start moving to wherever we clicked with the right mouse button.
Now, this is an interesting piece of game logic.
How do you direct a group of units?
You can't just send them all to the same location, or they'd end up overlapping.
For simplicity's sake, I chose to arrange them randomly in a square, with area proportional to the number of selected units.
```haskell
handleEvent (SDL.MouseButtonEvent (SDL.MouseButtonEventData _ SDL.Pressed _ SDL.ButtonRight _ (P (V2 px py)))) = do
  sl :: Slice Selected <- owners
  let r = (*3) . subtract 1 . sqrt . fromIntegral . S.size $ sl

  S.forM_ sl $ \e -> do
    dx <- liftIO$ randomRIO (-r,r)
    dy <- liftIO$ randomRIO (-r,r)
    set e (Target (V2 (fromIntegral px+dx) (fromIntegral py+dy)))

handleEvent _ = return ()
```
`owners` returns a `Slice` of all members that have that particular component.
A `Slice` is a list of entities.
The reason we need a slice instead of a map is that we need to know the amount of selected units.
`S.forM_` monadically iteraters over a `Slice`.
`set entity component` then explicitly writes a component for an entity, overwriting whatever might have been there.

#### Rendering
Rendering turns out to be really easy.
It looks like this:
```haskell
cimapM_ $ \(e, Position p) -> do
  e <- exists (cast e :: Entity Selected)
  liftIO$ SDL.rendererDrawColor renderer $= if e then V4 255 255 255 255 else V4 255 0 0 255
  SDL.drawPoint renderer (P (round <$> p))
```
`cmapM_` is to `cmap` as `mapM_` is to `map`.
Here we see `cimapM_`, note the extra `i`, which gives both the read component, and the current entity.
We then check whether or not it has a `Selected` component.
`exists :: Entity c -> System w ()` checks to see if the entity has a certain component.
We could emulate this with `get`, but this is, like `resetStore`, more general and usually faster.
Because the entities we iterate over are only guaranteed to have a `Position`, their type is `Entity Position`.
To check whether or not they are `Selected`, we need to explicitly cast them.
If you were to call `exists` with an `Entity (Position, Velocity)`, it'd tell you whether or not that entity has both a `Position` and `Velocity`.

#### Conclusion
These are the tools you need to build a game in apecs.
I did not discuss every line in the final program, as they were mostly SDL-related.
Again, the final version in its full glory can be found [here](https://github.com/jonascarpay/apecs/blob/master/examples/RTS.hs).
If you have any questions or suggestions, feel free to open an issue or PR.
