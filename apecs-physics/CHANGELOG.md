## [0.3.2]
### Changed
- Fixed links and added changelog to cabal file

## [0.3.1]
### Changed
- added `apecs` version bound

## [0.3.0]
### Added
- `ShapeList` and `ConstraintList` components for bodies, that contain a list of entity indices of their shapes and constraints (read-only).
### Changed
- `Shape`s, `Constraint`s, and `CollisionHandler`s now track their original Haskell representations, and can be meaningfully read.
- `Shape` and `Constraint` now only have a single constructor, that explicitly takes an entity argument indicating what entity it belongs to. Previously, the interface suggested that shapes and constraints were properties of bodies, which was wrong.
- Bodies now track their shapes and constraints in /mutable/ stores
### Removed
- The `ShapeBody` component has been removed. You can find out a shapes body by reading the `Shape` component's `ShapeExtend` constructor directly.
