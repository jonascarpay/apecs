# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands

```bash
# Build all packages
cabal build all
# or
stack build

# Run tests for the core package
cabal test apecs-test
# or
stack test apecs

# Run a single test module (QuickCheck-based, no named test selection)
cabal test apecs-test --test-show-details=always

# Run benchmarks
cabal bench apecs-bench       # original ECS system benchmarks
cabal bench store-bench       # per-store read/write throughput across access patterns
cabal bench world-bench       # multi-store world: iteration-order and cross-store write costs
# or
stack bench apecs

# Format code (fourmolu is configured via fourmolu.yaml)
fourmolu --mode inplace <file>
```

## Repository Structure

This is a multi-package Cabal project:

| Package | Purpose |
|---|---|
| `apecs/` | Core ECS library (the main package) |
| `apecs-physics/` | 2D physics via Chipmunk2D bindings |
| `apecs-gloss/` | Rendering frontend using gloss |
| `apecs-stm/` | STM-based stores for concurrency |
| `examples/` | Example games (Shmup tutorial, etc.) |

## Core Architecture

apecs is built around a small set of typeclasses in `Apecs.Core`:

- **`Component c`** — declares how a component type is stored via an associated type `Storage c`
- **`Has w m c`** — declares that world `w` can provide a `Storage c`; enables type-directed store lookup
- **`ExplGet/ExplSet/ExplDestroy/ExplMembers m s`** — the store interface; all system operations desugar to these
- **`SystemT w m`** — a `ReaderT w m` newtype; game logic runs in this monad

### Stores (`Apecs.Stores.Internal`)

Four concrete store types:
- `Map c` — `IntMap`-backed, O(log n), the default
- `Cache n s` — fixed-size vector cache wrapping another store, promotes reads/writes to O(1) using bitmask indexing; size rounds up to next power of two
- `Unique c` — holds at most one component across all entities
- `Global c` — one component shared by all entities; initial value is `mempty`; read/write via `global` (Entity -1)

`ReadOnly s` wraps a store to hide `ExplSet`/`ExplDestroy`; bypass with `setReadOnly`/`destroyReadOnly`.

### Pseudo-components (`Apecs.Components`)

These are implemented entirely as store wrappers with no real backing data:
- `Not c` — inverts existence; writing `Not` destroys the component
- `Maybe c` — optional component; always `explExists = True`
- `Either ca cb` — disjunction; gets prefer `Right`
- `Filter c` — tests existence without reading; equivalent to `Not (Not c)`
- `Entity` — echoes back the entity index; used in `cmap $ \(a, ety :: Entity) -> ...`

Tuple instances (2–8) are generated via TH in `Apecs.THTuples` and imported by `Apecs.Components`.

### World generation (`Apecs.TH`)

`makeWorld "World" [''Pos, ''Vel, ...]` generates:
- A record `data World = World !(Storage Pos) !(Storage Vel) ... !(Storage EntityCounter)`
- `Has World m Pos` instances (one per component) via `asks` on the record field
- `initWorld :: IO World` applying `explInit` to each field

`makeWorldAndComponents` additionally generates `Component` instances with `Map` storage.

### System combinators (`Apecs.System`)

The key combinators (`cmap`, `cmapM`, `cmapM_`, `cfold`, `cfoldM`) iterate over `explMembers` of the **first** component in a tuple — put the rarest component first for best performance.

### Experimental modules (`Apecs.Experimental.*`)

- `Reactive` — wraps a store to fire side-effects on set/destroy via `Reacts` typeclass; useful implementations: `Printer`, `EnumMap`, `OrdMap`, `IxMap`, `ComponentCounter`
- `Children` — parent/child entity relationships
- `Reload` — GHCi world persistence via `foreign-store`
- `Stores` — additional store types
- `Components` — additional pseudo-components
- `ArrayMap` — direct-indexed growing array; O(1) get/set/exists, O(capacity) members; aliases `ArrayMapB` (boxed) / `ArrayMapU` (unboxed, requires `Unbox`)
- `ChunkStore` — paged `IntMap` with fixed-size mutable leaf arrays (page size is a type-level `Nat`); O(log P) ops, O(E) members; aliases `ChunkStoreB` / `ChunkStoreU`
- `SparseStore` — sparse-set with packed dense array; O(1) ops, O(count) members with excellent iteration locality; aliases `SparseStoreB` / `SparseStoreU`

## Code Style

- Formatted with fourmolu (see `fourmolu.yaml`): 2-space indentation, leading commas, leading function arrows, multi-line Haddock
- `-Wall` enabled on all targets; benchmarks also use `-O2 -optlo-O3`
- Heavy use of `{-# INLINE #-}` pragmas on all store methods for performance
- GHC extensions used throughout: `TypeFamilies`, `FlexibleInstances`, `MultiParamTypeClasses`, `ScopedTypeVariables`, `TemplateHaskell`
