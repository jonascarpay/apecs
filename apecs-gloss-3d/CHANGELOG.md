# Changelog

## [0.1.0.0]
### Added
- `Apecs.Gloss3D`, extracted from the apecs-box3d demo: `Camera3` and
  `Env3` globals, per-frame `Scene3`, depth-tagged `Piece`s with
  `assemble`/`foldDraw3`, convex `Solid`s, and the `sphere3`/`solid3`/
  `wire3`/`hiddenWire3`/`blob3` primitives.
- `facesSolid`: build a `Solid` from face corner cycles, with normals
  computed by Newell's method and auto-oriented outward; `solidEdges`:
  the unique edges of a `Solid`, for `wire3`.
