## [0.3.0]
### Added
- `Shapes` and `Constraints` components for bodies, that contain a list of entity indices of their shapes and constraints.
### Changed
- `Shape`s, `Constraint`s and `CollisionHandler`s now track their original Haskell representations, and can be meaningfully read.
- Bodies now track their shapes and constraints in mutable stores
### Removed
- The `ShapeBody` component has been removed. You can find out a shapes body by reading the `Shape` component's `ShapeExtend` constructor directly.
