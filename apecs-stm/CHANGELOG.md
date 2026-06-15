## [0.3]

Reverting the 0.2 changes around `EntityCounter` and taking the codebase back to the 0.1.5 state.

### Added
- `newEntity_`, an STM counterpart of IO `newEntity_`.
- `TMap`, a map store backed by `TVar (IntMap (TVar c))`: an outer `TVar` for
  the live set plus a per-entity inner `TVar` for each value. Updating an
  existing component only touches its inner `TVar`, so concurrent value updates
  to different entities never conflict. Much cheaper per operation than the
  stm-containers backed `Map`. Compose with `Apecs.Sharded` (e.g.
  `Sharded 64 (TMap c)`) to also spread structural changes across shards. See
  the `apecs-stm-bench` benchmark for comparisons.

## [0.2]
### Removed
- Removed custom `EntityCounter`, `newEntity`, and `makeWorld`.
### Changed
- (#118) Use `makeWorld`, `EntityCounter` and entity functions from core.

## [0.1.5]
### Changed
- Bumped the minimum apecs version to 0.9.3, since we need `makeMapComponentsFor`

## [0.1.4]
### Add
- (#74) `makeWorldAndComponents`, similar to how it exists in apecs proper

## [0.1.3]
### Changed
- (#60) Add `Component` type names in non-existent component errors
- Increased upper bound `apecs` dependency

## [0.1.2]
### Changed
- apecs version bump

## [0.1.0]
### Added
- Split out the STM parts of the main apecs package
