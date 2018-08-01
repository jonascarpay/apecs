## [Unreleased]
### Changed
- `System w a` is now a synonym for `SystemT w IO a`.
  Most of the library has been rewritten to be as permissive as possible in its monad argument.
  A variable monad argument allows apecs to be run in monads like ST or STM.

## [0.4.1.2]
### Changed
- Either can now be deleted, deleting `Either a b` is the same as deleting `(a,b)`.
- Some were missing their inline pragma's, now they don't

## [0.4.1.1]
### Changed
- Export `Get`, `Set`, `Destroy`, `Members` by default
- Export `cfold`, `cfoldM`, `cfoldM_` by default
- Fix () instance

## [0.4.1.0]
### Added
- `cfold`, `cfoldM`, `cfoldM_`
- `Either` instances and `EitherStore`

### Changed
- Changed MaybeStore implementation to no longer use -1 for missing entities.
- Fixed some outdated documentation.
- Change the `global` void entity to -2, just to be sure it won't conflict if accidentally used in a cache.

## [0.4.0.0]
### Added
- A changelog

### Changed
- `Store` is now split into 5 separate type classes; `ExplInit`, `ExplGet`, `ExplSet`, `ExplDestroy`, and `ExplMembers`.
    This makes it illegal to e.g. iterate over a `Not`.
- phantom arguments are now given as `Proxy` values, re-exported from `Data.Proxy`. This makes phantom arguments explicit and avoids undefined values.
