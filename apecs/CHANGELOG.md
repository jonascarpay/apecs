## [0.9.6]
### Changed
- (#110) Relax upper bound on `mtl`: 2.3 -> 2.4
- (#117) Fix TH symbol leaking, fixing (#116)
- (#121) Properly export `Printer` from `Apecs.Experimental.Reactive`
- (#121) Fix haddocks for `Apecs.Experimental.Reactive`
- (#125) Force `IntMap` in `ExplDestroy` instance for `Map`
- (#128) Use `SystemT` instead of `System` in `runGC` type signature
- (#131) Enable `-XTypeOperators` to prevent GHC warnings
### Added
- (#121) `ComponentCounter`
- (#123) `SystemT` `MonadUnliftIO` instance
- (#126) Export `cmapIf` from main `Apecs` module
- (#130) Docs for performance considerations when reading composite components 
- (#132) `Apecs.Experimental.Children`
### Removed
- (#112) Custom setup for C sources

## [0.9.5]
### Added
- (#99) `collect`
### Changed
- (#84) Updated links to articles

## [0.9.4]
### Changed
- (#86) Add support for GHC 9.2 and Template Haskell 2.18

## [0.9.3]
### Added
- `newEntity_ = void . newEntity`

## [0.9.2]
### Changed
- (#68) Add instances of MonadThrow, MonadCatch, and MonadMask to SystemT
- Cleaned up the README
- Small haddock fixes

## [0.9.1]
### Changed
- (#63) Fixed bug where `modify` on non-existent components crashes

## [0.9.0]
### Added
- (#59) Expose `makeMapComponents`, which creates `Component` instances with `Map` stores
### Changed
- (#60) Add `Component` type names in non-existent component errors
- Relaxed the type of `modify` to allow a different return type
- Constrain the `cmapM_` type `(c -> SystemT w m ()) -> SystemT w m ()`, to make it clearer that the inner function does not update `Components`
- Simplify nix infrastructure

## [0.8.3]
### Changed
- (#58) Added support for Template Haskell 2.15.0.0 through CPP flags

## [0.8.2]
### Changed
- (#55) Fixed a bug where components where not properly deleted from the cache following the cache bitmasking update

## [0.8.1]
### Changed
- Changed `Cache`s to use bitmasks instead of the remainder operation. This makes caches up to three times faster.
- Fixed bug in `Cache` where the same entity appeared in member list of both the cache and the underlying store

## [0.8.0]
There are a number of unsolved problems in apecs' design space.
Most notably, it needs a good way to do streaming and reactivity, or find a way to integrate with existing solutions.
I'm hesitant to accept some of the feature requests I've gotten because they would be obsoleted when we figure this out, and I don't want to pollute the API with unstable features.

However, I don't think we should let perfect get in the way of good.
So, apecs 0.8 have new `Apecs.Experimental.*` modules, that I want to use for features that might or might not get removed or changed.
They should provide some conveniences, but the catch is that their API might undergo significant changes between point releases (and therefore within LTS'es).
Some of the existing modules were already marked experimental, and those have been moved.

### Added
- Experimental `Head` component
- `OrdMap` and `IxMap` reactive maps

### Changed
- Moved `ReadOnly` to `Apecs.Stores` since `EntityCounter` now depends on it
- Moved spatial hashing to `Experimental.Util`
- Moved `Redirect` and `Stores.Extra` to `Experimental.Stores`
- Moved `Reactive` to `Experimental.Reactive`
- `rget` has been replaced by `withReactive`
- Improved error messages for unsafe operations

## [0.7.3]
### Changed
- Added Data.Semigroup to Stores.Extra to build with GHC 8.2.2 in hackage matrix

## [0.7.2]
### Changed
- Fixed bug in the `Pushdown` store
- `Apecs` module no longer re-exports the entire `Data.Proxy` module, but instead just `Proxy (..)`.
- Added (approximate?) lower and upper version bounds to dependencies

## [0.7.1]
### Added
- `$=` and `$~` operators as synonyms for `set` and `get` respectively
### Removed
- `getAll` and `count`, which were made redundant by `cfold`.

## [0.7.0]
### Added
- The `Reactive` store and module is a redesign of the `Register` store, and provides a more general solution for 'stores that perform additional actions when written to'.
- The `Apecs.Stores.Extra` submodule, which contains the `Pushdown` and `ReadOnly` stores. `Pushdown` adds pushdown semantics to stores, and `ReadOnly` hides the `ExplSet` instances of whatever it wraps.
- The `EntityCounter` and associated functions have all been specified to `IO`, since `Global EntityCounter` only works in IO. Furthermore, `EntityCounter` now uses a `ReadOnly` store, to prevent users from accidentally changing its value.
- `Redirect` component that writes to another entity in `cmap`.
### Changed
- Default stores have `MonadIO m => m` instances, rather than `IO`. This makes it easier to nest `SystemT`.
- All apecs packages have been consolidated into a single git repo.
- `Apecs.Components` contains the components (and corresponding stores) from `Apecs.Core`.

## [0.6.0.0]
### Changed
- Nothing, but since 0.5.1 was API-breaking I've decided to bump to 0.6
## [0.5.1.1]
### Changed
- `Register` needs UndecidableInstances in GHC 8.6.2, I'm looking for a way around this. I've removed it for now.

## [0.5.1.0]
### Added
- The `Register` store, which allows reverse lookups for bounded enums.
  For example, if `Bool` has storage `Register (Map Bool)`, `regLookup True` will yield a list of all entities with a `True` component.
  Can also be used to emulate a hash table, where `fromEnum` is the hashing function.
  This allows us to make simple spatial hashes.
  I'm open to suggestions for better names than Register.
- `cmapIf`, cmap with a conditional test
### Changed
- `ExplInit` now too takes a monad argument.
- Started rewrite of the test suite
- Caches now internally use -2 to denote absence, to avoid possible conflict with -1 as a global entity
### Removed
- The STM instances have been removed, to be moved to their own package

## [0.5.0.0]
### Changed
- `System w a` is now a synonym for `SystemT w IO a`.
  A variable monad argument allows apecs to be run in monads like ST or STM.
  Most of the library has been rewritten to be as permissive as possible in its monad argument.
### Added
- STM stores. These will be moved to a separate package soon.

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
