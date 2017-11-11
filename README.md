# phycs

2D game physics.
Uses [Chipmunk2D](https://github.com/slembcke/Chipmunk2D) for fast physics and [apecs](https://github.com/jonascarpay/apecs) for object management.

Also provides tools for [gloss](https://github.com/benl23x5/gloss)-based rendering.

WIP. Do not use this yet.

### Example
```haskell
makeWorld "World" [''Color, ''Physics]

initialize = do
  writeGlobal (Gravity (V2 0 (-10)))

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

![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/img.png)
