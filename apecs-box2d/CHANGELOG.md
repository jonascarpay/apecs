# Changelog

## [0.1.0.0]
### Added
- `Apecs.Box2D`, modelled on apecs-physics: the `Physics` world
  component with `Gravity`, `Substeps` and `destroyPhysics`, the
  `Body` component with
  `Position`/`Velocity`/`Angle`/`AngularVelocity`/`BodyMass`/`Force`/
  `Torque` sub-components, the `Shape` component (circle, capsule,
  segment, box, convex polygon) with `Density`/`Friction`/`Elasticity`,
  and the
  `B2BodyId`/`B2ShapeId`/`getWorldId` escape hatches to the raw engine.
- The `Joint` component (pivot, distance, weld, spring, slide, rotary
  spring/limit, motor; world-space specs) with the `B2JointId` escape
  hatch.
- `apecs-box2d-gallery`: a joint gallery after the apecs-physics
  Constraints example.
- Body dynamics extras: `LinearImpulse`/`AngularImpulse` appliers and
  `LinearDamping`/`AngularDamping`/`GravityScale`.
- `CollisionFilter` on shapes, re-exporting `Filter`.
- World queries: `segmentQuery` (closest hit along a segment as a
  `RayHit` with the shape and body entities), `aabbQuery` and
  `pointQuery` (broad-phase box overlap returning body entities).
- Collision events: `Collisions` (begin-touch pairs) and `Impacts`
  (hits with contact point, normal and approach speed) as read-only
  globals reflecting the last `stepPhysics`; shapes created by the
  layer opt into both event kinds.
- `BulletBody`: continuous collision detection toggle — without it,
  small fast bodies tunnel through dynamic targets between substeps.
- Debug drawing: `debugDraw` runs the engine's debug renderer over the
  store's world through the upstream `Draw` callbacks, and
  `debugDrawCommands` records a frame as a pure `[Cmd]` list to render
  after the world is free again.
- `apecs-box2d-tumbler`: a tumbler scene driven by Box2D and rendered with
  apecs-gloss.
