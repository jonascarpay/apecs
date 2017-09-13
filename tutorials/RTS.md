### apecs tutorial - An RTS-like game

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
Game logic is expressed in systems that operate on the game world.

What makes most ECS fast is that we can store components of the same type together.
In fact, by storing each component together with the ID of the entity it belongs to, an entity becomes an implicit collection altogether.
In apecs, what components an entity consists of is dictated entirely at the type level.

If this all seems confusing, don't worry.
Things will probably become clearer once you follow the tutorial.

#### Components
We start by defining our components.

First up is position.
A position is just a two-dimensional vector of double:
When defining a data type as a component, you have to specify how the component is stored in memory.
At the root of a storage you'll generally find one of three kinds of storage; a `Map`, `Set`, or `Global`.
In this case, we can simply store the position in a `Map`.
```haskell
newtype Position = Position {getPos :: V2 Double} deriving (Show, Num)

instance Component Position where
  type Storage Position = Map Position
```

A target is whatever position the entity is moving towards.
Again, the storage is a simple `Map`
```haskell
newtype Target = Target (V2 Double)

instance Component Target where
  type Storage Target = Map Target
```

We use `Selected` to tag an entity as being currently selected by the mouse.
We can designate `Selected` as being a flag by defining a Flag instance, which gives us access to the `Set` storage.
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
In fact, you'll find that the bottleneck is SDL, and not apecs.


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
It simply holds the storages of each component.
Or, to be more precise, it holds immutable references to mutable storage containers for each of your components.
When actually executing the game, we produce a world in the IO monad:
```haskell
initWorld = do
  positions <- initStore
  targets   <- initStore
  selected  <- initStore
  mouseState <- initStoreWith Rest
  counter <- initCounter
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
