# phycs

2D game physics.
Uses [Chipmunk2D](https://github.com/slembcke/Chipmunk2D) for fast physics and [apecs](https://github.com/jonascarpay/apecs) for object management.

Also provides tools for [gloss](https://github.com/benl23x5/gloss)-based rendering.

WIP. Do not use this yet.

### Guided tour

#### Hello World
`stack build && stack exec helloworld`
```haskell
makeWorld "World" [''Color, ''Physics]

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( DynamicBody
            , Position (V2 0 2)
            , Shape (Circle 0 0.5) defaultProperties {friction = 1} )

  newEntity ( StaticBody
            , Shape (Segment (V2 (-3) 0) (V2 3 0) 0) defaultProperties {friction = 1}
            , Angle (-pi/10) )
  return ()

main = simulateWorld (InWindow "helloworld" (640,480) (10,10)) 40 initWorld initialize
```
![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/examples/helloworld.png)

#### Tumbler
`stack build && stack exec tumbler`
```haskell
makeWorld "World" [''Color, ''Physics]

initialize = do
  setGlobal (Gravity (V2 0 (-10)))

  newEntity ( KinematicBody
            , AngularVelocity (-pi/6)
            , hollowBox 30 30 0 defaultProperties )

  replicateM_ 400 $ do
    x      <- liftIO$ randomRIO (-9,9)
    y      <- liftIO$ randomRIO (-9,9)
    radius <- liftIO$ randomRIO (0.4,0.8)
    let color = (realToFrac x+9)/19

    newEntity ( DynamicBody
              , Shape (Circle 0 radius) defaultProperties {elasticity=0.9}
              , Position (V2 x y)
              , makeColor 1 color color 1 )

main = simulateWorld (InWindow "phycs" (640,480) (10,10)) 10 initWorld initialize
```

![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/examples/tumbler.png)

#### Chain
`stack build && stack exec chain`
```haskell
makeWorld "World" [''Color, ''Physics]

initialize = do
    setGlobal $ Gravity (V2 0 (-10))
    fixed <- newEntity StaticBody
    chain (cast fixed) 0
  where
    links = 10
    chain a n = do
      b <- newEntity ( DynamicBody
                     , Position (V2 n 5)
                     , Shape (Segment 0 (V2 1 0) 0.1) defaultProperties )

      newEntity $ Constraint (cast a) (cast b) (PivotJoint (V2 n 5))
      unless (n >= links) $ chain b (n+1)

main = simulateWorld (InWindow "chain" (640,480) (10,10)) 30 initWorld initialize
```

![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/examples/chain.png)
