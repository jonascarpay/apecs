## [0.3]

Reverting the 0.2 changes around `EntityCounter` and taking the codebase back to the 0.1.5 state.

### Added
- `newEntity_`, an STM counterpart of IO `newEntity_`.

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
