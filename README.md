# phycs

2D physics engine.
Uses [apecs](https://github.com/jonascarpay/apecs) and [Chipmunk2D](https://github.com/slembcke/Chipmunk2D).

Also provides tools for [gloss](https://github.com/benl23x5/gloss)-based rendering.

WIP.

### Example
```haskell
makeWorld "World" [''Physics]

initialize = do
  writeGlobal (Gravity (V2 0 (-10)))

  let ball = Shape (Circle 0 0.2) defaultProperties {elasticity = 0.8}
      line = Shape (Segment (V2 (-1) 0) (V2 1 0) 0) defaultProperties {elasticity = 0.8}

  newEntity (DynamicBody, shape ball, Position (V2 0 2))
  newEntity (StaticBody,  shape line, Position (V2 0 (-1)), Angle (-pi/10))

main = simulateWorld (G.InWindow "phycs" (640,480) (10,10)) 100 initWorld initialize
```

![Screenshot](https://raw.githubusercontent.com/jonascarpay/phycs/master/img.png)
