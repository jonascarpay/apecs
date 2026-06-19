## [0.3]

The package is now fully subordinate to vanilla apecs,
reusing its EntityCounter, makeWorld etc.

While it can still be used as "put everything into STM"
this is quite inefficient.

* Renamed Map, Global, Unique to TMap, TGlobal, TUnique.
  They now can be used alongside vanilla Map/Global/Unique
  and only where it matters.
* TMap.explMembers is not atomic under IO.
  Using stm-containers' listTNonAtomic escape hatch for
  single-threaded/non-atomic membership tests.

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
