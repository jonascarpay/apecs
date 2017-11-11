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

  let ball = Shape (Circle 0 0.2) defaultProperties {elasticity = 0.8}
      line = Shape (Segment (V2 (-1) 0) (V2 1 0) 0) defaultProperties {elasticity = 0.8}

  newEntity (DynamicBody, Shapes [ball], Position (V2 0 2),    red)
  newEntity (StaticBody,  Shapes [line], Position (V2 0 (-1)), Angle (-pi/10))

main = simulateWorld (InWindow "phycs" (640,480) (10,10)) 100 initWorld initialize
```

![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/img.png)
