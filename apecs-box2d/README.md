# apecs-box2d

[apecs](https://hackage.haskell.org/package/apecs) integration for the
[Box2D](https://box2d.org/) physics engine, via the
[`Box2D`](https://gitlab.com/dpwiz/box-nd) bindings.

The API follows apecs-physics: add `Physics` to your world, give
entities a `Body` component, and read or write the engine-backed
sub-components (`Position`, `Velocity`, `Angle`, `Shape`, `Density`,
...) with regular apecs systems. The simulation state lives inside
Box2D rather than being mirrored into apecs stores; the raw engine
stays reachable through `B2BodyId`, `B2ShapeId`, `B2JointId` and
`getWorldId` — add `Box2D` to your own build-depends to import the raw
modules those ids unlock.

## Tutorial

New to Box2D (or to this integration)? The [arena tag
tutorial](tutorial/README.md) builds a small top-down game from
scratch — bodies, forces, teleports, ray casts and impact events —
assuming only apecs familiarity.

```sh
stack run apecs-box2d-tutorial
```

## Demo

`apecs-box2d-demo` is a Box2D take on the apecs-physics tumbler,
rendered with [apecs-gloss](https://hackage.haskell.org/package/apecs-gloss):
a spinning kinematic box full of bouncing balls. Click to spawn more.

```sh
stack run apecs-box2d-demo
```

`apecs-box2d-gallery` exhibits the joint kinds on a grid, after the
apecs-physics Constraints example: spring, rod, slide, pivot chain,
rotary spring, rotary limit, motor, and weld. Click to drop boxes.

```sh
stack run apecs-box2d-gallery
```
