# apecs 1.0

This is an (experiment for) the next version of `apecs`.
It completely overhauls the internals, and some of the externals as well.
See below for more information about what, how, and why.

## Todo
  - [ ] Stores
    - [X] Cache
      - [ ] polymorphic in backing vector
        I want this so I can have Storable vectors for things I want to send to the GPU
      - [ ] Type error when the cached component does not
        actually exist in the cached store
  - [ ] Module structure
    - [ ] Rename Focus
  - [X] Benchmarks
    - [ ] Profiling comparison
    - [ ] Performance tweaks
  - [ ] Stack support
  - [ ] Redesign newEntity/EntityCounter?
    - [ ] Make `newEntity` more general?
    - [ ] Make `EntityCounter` less of a special case?
  - [ ] Documentation/blog posts
    - [ ] How to use
    - [ ] How to go fast
    - [ ] Internals
  - [ ] Feature parity
    - [ ] Stores
    - [ ] Systems
    - [ ] Some things might need to be redesigned
      - [ ] Reactivity?
    - [ ] Some things might no longer be needed
  - [ ] Clean up Nix
  - [ ] CI
  - [ ] Haddocks
  - [ ] Tests
  - [ ] port secondary libraries
    - [ ] apecs-gloss
    - [ ] apecs-physics
    - [ ] apecs-stm
  - [ ] Fix version bounds
  - [ ] Everything else
  - [ ] Open questions
    - [ ] Multi-component stores?
      - [ ] What else?
      - [ ] Custom type errors?
        - [ ] No Generic or more specific instance
        - [ ] `Components` undefined for World member
        - [ ] probably others
    - [ ] SystemT: Synonym or newtype?

## More information

This is the next version of apecs.
The reason I'm (tentatively) calling it 1.0 is that it is _so_ different from 0.9 that calling it 0.10 doesn't really do it justice.

In short, the difference between this and 0.9 is that this version uses Generics where 0.9 used Template Haskell.
The surface level of the library mostly stays the same, but the implementation is a lot simpler.

This came about while I was looking at `generic-lens`.
In the past, I tried writing apecs using Generics a number of times, but I was never able to really make it work out in a way I was happy with.
However, `generic-lens` showed me how to do many of the things I was struggling with, and that it can be just as fast as handwritten/TH code.

It took a while to get to a point where it was an improvement over TH, now that it has, I think it is time to get feedback.

### Changes
Example.hs contains a rewrite of the example in the apecs readme:

```haskell
newtype Position = Position Float deriving (Eq, Show)

newtype Velocity = Velocity Float deriving (Eq, Show)

data World
  = World
      (Map Position)
      (Map Velocity)
      EntityCounter
  deriving (Generic, Initialize IO)

main :: IO ()
main = do
  (w :: World) <- initialize
  flip runReaderT w $ do
    newEntity (Position 0, Velocity 3)
    newEntity (Position 1, Velocity 4)
    newEntity (Position 2, Velocity 5)
    cmap $ \(Position p, Velocity v) -> Position (p + v)
    cmapM_ $ \(Position p) -> liftIO $ print p
```

Instead of providing component declarations and generating the world from them, you now write the world yourself.
As you can see in `main`, the mechanics of using apecs mostly stay the same, but defining components and the world is a lot more transparent.
This makes it so that you can manipulate the `World` directly if you want, like making copies.

At its core, apecs is about 4 type classes; `Get`, `Set`, `Destroy`, and `Members`.
This separates the concerns of the front-end (providing a safe, high-level language) the backend (fast memory access).
Previously, there were a number of extra classes that were necessary to make this actually work out though, like `Has`, and the `Expl...` family.
Apecs 1.0 manages to do away with the noise however, and the core module truly only consists of the 4 core type classes.
All the heavy lifting is done by the instances.

The most "magical" of these instances are those that allow a Generic world to inherit the instances of its constituent stores.
This is the part that was copied from/inspired by `generic-lens`.
There are open questions about this part of the design space.
For example, there is first-class support for stores that contain multiple components.
This is nice for `apecs-physics` and reactive stores, but it makes for particularly bad error messages.
We could write custom type errors, but we'll have to talk about whether it's worth the complexity.

### Performance

The performance numbers are similar to 0.9.
It appears that some things are slightly faster, and some things are slightly slower.
Profiling or more benchmarks would be nice, and there might be more gains to be made, but it's good to know we're still fast.

### Ecstasy

I've been asked before about the differences between apecs and ecstasy.
Ecstasy brands itself as apecs, but with Generics, so you'd think that now that apecs is moving towards Generics they're converging, right?

In practice however, apecs and ecstasy are so different it's hard to even compare them.
They were already very different when ecstasy was first released, but apecs itself has evolved so much it looks nothing like it did back then.

I can't tell you which of the two is right for you; my experience with ecstasy is limited to studying its source code, and I'm obviously at least a little bit biased.
That said, I have studied ECS systems a lot, and wrote an (unpublished) paper on them.
In my opinion, the fact that apecs has a small, simple core language puts its design above that of every other ECS system I know.
It also happens to be fast, and that makes for great marketing, but it's mostly just a consequence of having a solid core.
That obviously doesn't make it the best choice for everyone, though; for example, Entitas or DOTS are both embedded in Unity, and therefore part of an industry-standard game development ecosystem.

Take that however you will.
It's great that there are multiple efforts in ECS development, especially when they're so different.
ECS systems are young, and any development is good development.
