## apecs tutorial
### Making games fast

As promised, a guide on how to make games fast.
The reason for this tutorial is not that you need to know all of this in order to use apecs.
For simple 2D games, apecs won't be a bottleneck.
Once you start worrying about 3D and render times however (check out the amazing [gpipe](https://github.com/tobbebex/GPipe-Core)), that's when you might need apecs to be fast, and this is how you do that.

When describing Entity Component Systems, they are generally presented as a useful tool for expressing game logic.
What makes ECS so cool is that they can do this while still being really fast.
Apecs' performance is competitive with that of ECS implementations in Rust.
The reason for this is twofold:

First, ECS are a bad fit for most programming languages.
What I mean by that is that they require a lot of runtime infrastructure between the DSL and the actual component storages, which makes them slow (and often verbose).
I don't mean to disparage any piece of software, I just mean that I know of no implementation that can yet approach the speed of handwritten C/C++/Rust.
ECS are still _really_ fast, and I suspect for most use cases their benefits far outweigh their costs.
All I want to say is this: the bar for apecs to be fast is not as high as you might think.

Second, Haskell turns out to be particularly suited for writing an ECS in.
This might surprise you, as Haskell is garbage collected, functional, lazy by default, pure, and only has immutable values.
The reason Haskell gets to be fast is that Haskell's type system allows an ECS to be defined in terms of small primitive functions and corresponding semantics.
This is good, first of all, because GHC is really good at turning compositions of small functions into fast code.
The second reason is that we can generically define common operations in terms of those primitives.
Stores can then provide optimized versions of those operations, provided they are functionally equivalent, and the compiler will then pick those optimized versions at compile time.
The final reason is that we can compose Stores, as long as we promise to preserve their semantics, which is where caches come from.

All of this means that a lot of the infrastructure I mentioned previously gets generated at compile time, and then optimized into fast specialized code.
Add a bit of monad magic and voilà, fast stateful imperative programming in Haskell.

#### Stores
Let's take a look under the hood.
This is the `Store` class minimal complete definition:
```haskell
class Store s where
  type InitArgs s
  type Stores s
  type SafeRW s

  explGet     :: s -> Int -> IO (SafeRW s)
  explSet     :: s -> Int -> Stores s -> IO ()
  explDestroy :: s -> Int -> IO ()
  explMembers :: s -> IO (U.Vector Int)

  explGetUnsafe :: s -> Int -> IO (Stores s)
  explSetMaybe  :: s -> Int -> SafeRW s -> IO ()

  initStoreWith :: InitArgs s -> IO s
```
Almost every `expl` function has a user-facing version, like `explGet` and `get :: Entity c -> System w (Safe c)`.
Not all of the code is important for now.
A store is mostly expected to do 4 things:

1. `explGet` takes an index (the ID of some entity) and maybe returns a component for that index.
2. `explSet` takes an index and a component, and then stores that component somewhere.
3. `explDestroy` removes a component for some index if it held one.
4. `explMembers` yields a list of all members.

All systems are/can be expressed in terms of these functions.
It might seem like I'm going a little far into the details here, after all, this mostly comes down to that Stores behave like maps/dictionaries/sets*, but understanding the way these functions compose is useful for understanding performance and writing your own `Stores`.

#### Tuples
Tuples have `Component` instances (see other tutorials) because tuples have `Store` instances.
These mostly behave as you'd expect:
```haskell
explSet (sa,sb) ety (wa,wb) = explSet sa ety wa >> explSet sb ety wb
```
But where it gets interesting is iterating over a tuple:
```haskell
explMembers (sa,sb) = explMembers sa >>= filterM (explExists sb)
```
When we iterate over a tuple, we take a list of all members of the first store, and filter out the elements that are not in the second.
The reason this matters is that it means that `cmap (f :: (Player, Position) -> (Player, Position))` is faster than `cmap (f :: (Position, Player) -> (Position, Player))` if there is only a single `Player`, but many entities with a `Position`.
The first example, internally, retrieves the player, checks if he has a position, and then applies the function.
The second example iterates over all positions, checks if they have a player, and applies the function.
When using tuples, just make sure that the rarest component is in the first position, and you're good.

#### Maps
`modify` is defined like this:
```haskell
explModify :: s -> Int -> (Stores s -> Stores s) -> IO ()
explModify s ety f = do
  etyExists <- explExists s ety
  when etyExists $ do
    r <- explGetUnsafe s ety
    explSet s ety (f r)
```
and `cmap` like this:
```haskell
explCmap :: s -> (Stores s -> Stores s) -> IO ()
explCmap s f = do
  sl <- explMembers s
  forM_ sl $ \ety -> explModify s ety f
```
These are the default implementations.
They are general, but because of that, relatively slow.
Stores will usually provide custom implementations that are a lot faster.
The issue is that these faster versions can no longer be used when you map over a tuple; you cannot express a map of a tuple as a tuple of maps.
The compiler will default to the general version, and the map will be a bit slower.

Don't let this discourage you from using tuples.
Just remember that maps over a single component are extra fast, so don't split components if you only ever address them together.

Take a look at the `Store` class to see what other functions have optimized versions.

#### Caches
This map is cached:
```
Cache 100 (Map Position)
```
What this means is that every time this map is written to, the component will be written to a cache instead.
A cache is a mutable vector of components of a fixed size, and hence writing and reading from one is _fast_.
Each component is assigned a place in the vector, and if there's a different component there already that component is pushed through to the `Map`.
Caches are what make Stores fast.

The `100` type literal indicates the cache size (note that you need -XDataKinds for this).
The larger the cache, the less often the collisions above will occur.

Don't make your caches excessively large however, as iterating over a cache takes time proportional to the cache size.
Even a small cache can provide a big performance boost, so don't obsess to find the perfect cache size.

Note that only `Maps` and other `Caches` can be cached.

#### Parallelism
Parallelism comes in two flavors.

1) `pcmap n` is a parallel version of `cmap`.
   This is achieved by getting a slice of all members, splitting it into pieces of `n` each, and handing each to a separate thread.
   This is a safe and easy way of achieving parallelism.
   Note that actually splitting the list and scheduling threads costs time, so generally only use this function it the mapped function takes a lot of time.

2) `concurrently` runs a list of systems in parallel, and waits for them to finish.
   There is no protection against race conditions, so use with caution.

#### Logs
The final key to performance is the correct use of `Logs`.
This map is logged by an `EnumTable`:
```
type Storage UnitType = Logger EnumTable (Map UnitType)
```
A pure log is defined by the following functions:
```haskell
class PureLog l c where
  pureEmpty :: l c
  pureOnSet     :: Entity a -> Maybe c -> c -> l c -> l c
  pureOnDestroy :: Entity a -> c -> l c -> l c
```
There's also a mutable equivalent called `Log l c`.
`Logger` only accepts `Logs`, you can turn a `PureLog` into a `Log` using `FromPure`.
You can basically slap a Log onto anything that can be cached.

A log is a piece of data that is updated when the underlying store is changed.
`onSet` takes the entity in question, maybe its previous value, and its new value, and updates the log.
`onDestroy` is the corresponding function for when an entity is destroyed.
With just these functions, you can track many interesting pieces of game state for only the cost of incremental updates.

`EnumTable` is currently the only included logger, but luckily it's very powerful.
Imagine you have a `data UnitType = Cat | Dog | Horse deriving (Enum, Bounded)`.
You need a slice of all `Cats`.
What you would normally do is take a slice of all `UnitTypes` and filter out non-cats.

For every `Enum` element, an `EnumTable` tracks what entities have that element.
In the example above, the `EnumTable` contains three sets of integers, one for each `UnitType`.
We can query an `EnumTable` in two ways:
Using `byIndex :: EnumTable c -> Int -> System w (Slice c)` and using `byEnum :: Enum c => EnumTable c -> c -> System w (Slice c)`.
You can get your log out of the game world using `getLog`.

Now, let's cheat a little.
In games with physics, you often want to keep a spatial hash of your entities.
A spatial hash divides the space into regions, and tracks what entities are in what region.
This allows you to perform spatial queries, which comes up a lot in things like collision detection.
Without a spatial hash, you often have no choice but to test the collisions for every possible pair of entities.

Here's how we make one in apecs:
```haskell
newtype Position = Position (V2 Double)
instance Component Position where
  type Storage Position = Logger EnumTable (Map Position)

instance Bounded Position where
  minBound = Position 0
  maxBound = Position 1
instance Enum Position where
  fromEnum (Position p) = flatten' 100 . quantize 0.01 $ p
```
That's it!
`flatten'` and `quantize` are two of a number of helper functions for spatial hashing found in `Apecs.Util`.
You can now use `region` and `byComponent` to get a list of all `Positions` in a certain area.

#### Conclusion
That concludes our tour of the apecs internals and performance tips.
Some final tips:

1. If you can, compile your program with `-fllvm -optlo-O3` for an extra 10-30% performance boost
2. You probably want to call runGC before or after every frame.
   This makes sure you don't build up unnecessary garbage, and keeps frame times consistent.

I hope I have answered any other performance related questions you might have had.
If not, let me know, and I'll see if I can cover it in more detail.

Contributions to apecs in the form of criticism/issues/pull requests are very welcome.
