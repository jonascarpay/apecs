# apecs-box3d

[apecs](https://hackage.haskell.org/package/apecs) integration for the
Box3D physics engine, via the
[`Box3D`](https://gitlab.com/dpwiz/box-nd) bindings — the 3D sibling of
apecs-box2d, with the same design: add `Physics` to your world, give
entities a `Body`, and work with the engine-backed sub-components
(`Position`, `Velocity`, `Rotation`, `Shape`, ...). Vectors are `Vec3`
and rotations are quaternions. The raw engine stays reachable through
`B3BodyId`, `B3ShapeId`, `B3JointId` and `getWorldId` — add `Box3D` to
your own build-depends to import the raw modules those ids unlock.

## Demos

`apecs-box3d-demo` is the tumbler one dimension up: a kinematic box
cage tumbling about two axes, full of spheres and cubes, rendered with
apecs-gloss-3d (painter's-algorithm depth sort across bodies, backface
culling within each convex cube, flat shading and distance fog) inside
an apecs-gloss window. Click to drop in more debris.

```sh
stack run apecs-box3d-demo
```

`apecs-box3d-elite` is a kinetic chase through an asteroid field,
flat-shaded: a Courier runs, two Eagles pursue, and
everything is autopiloted — each ship is a rigid body flown by a small
PD controller writing `Torque` and `Force`, and the guns fire real
dynamic bodies (with `BulletBody` continuous collision), so hits are
momentum transfer resolved by the engine — and the step's `Impacts`
despawn spent slugs and flash explosions at the contact points.
A classic scanner ellipse at the bottom shows contacts as height
lollipops. Click to scramble another Eagle.

```sh
stack run apecs-box3d-elite
```
