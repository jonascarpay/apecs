# Changelog

## [0.1.0.0]
### Added
- `Apecs.Box3D`, mirroring apecs-box2d in 3D: the `Physics` world
  component with `Gravity`, `Substeps` and `destroyPhysics`, the `Body`
  component with `Position`/`Velocity`/`Rotation`/`AngularVelocity`/
  `BodyMass`/`Force`/`Torque` sub-components, the `Shape` component
  (sphere, capsule, box, convex hull) with
  `Density`/`Friction`/`Elasticity`, and the
  `B3BodyId`/`B3ShapeId`/`getWorldId` escape hatches to the raw engine.
- The `Joint` component (spherical pivot, distance, weld; world-space
  specs) with the `B3JointId` escape hatch.
- Body dynamics extras: `LinearImpulse`/`AngularImpulse` appliers and
  `LinearDamping`/`AngularDamping`/`GravityScale`.
- `CollisionFilter` on shapes, re-exporting `Filter`.
- World queries: `segmentQuery` (closest hit along a segment as a
  `RayHit` with the shape and body entities), `aabbQuery` and
  `pointQuery` (broad-phase box overlap returning body entities).
  Note: Box3D's default shape category is all bits set, so give shapes
  explicit `CollisionFilter` categories to partition them for queries.
- Collision events: `Collisions` (begin-touch pairs) and `Impacts`
  (hits with contact point, normal and approach speed) as read-only
  globals reflecting the last `stepPhysics`; shapes created by the
  layer opt into both event kinds.
- `BulletBody`: continuous collision detection toggle — without it,
  small fast bodies tunnel through dynamic targets between substeps.
- Debug drawing: `debugDraw` runs the engine's debug renderer over the
  store's world through the upstream `Draw` callbacks, and
  `debugDrawCommands` records a frame as a pure `[Cmd]` list to render
  after the world is free again. Shapes need debug-shape callbacks in the
  `WorldDef`, which the store does not install yet, so only the overlays
  come out of a plain `initPhysics` world.
- `apecs-box3d-tumbler`: a 3D tumbler rendered with apecs-gloss via a CPU
  perspective projection and painter's-algorithm depth sorting.
- `apecs-box3d-elite`: an autopiloted kinetic chase through an asteroid
  field of flat-shaded convex hulls — PD-controller ships flown with
  `Torque`/`Force`, guns firing real dynamic bodies. A headless `film`
  mode exports exact frames to PNG via gloss-export for verification.
