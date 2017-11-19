This is how I keep track of the issues I still need to address.
If you want to help, I think it's easiest if you create an issue for the one you are fixing and assigning yourself, but I'm open to ideas.

The name of the game is mostly to identify Chipmunk objects, create a safe wrapper, and then implement `Store (Space component)` instances for that object.

### Need
- [x] Make separate example executables
- [x] Constraints
- [x] Collision Handling
- [ ] CollideBodies component for Constraints, currently hardcoded to never collide
- [x] Remaining Collision callbacks
- [x] Add Gloss `play` version to allow interactive games
- [ ] Implement body force components
- [ ] Implement remaining body components
- [ ] Implement remaining global properties
- [ ] Implement remaining constraints
- [ ] Free FunPtrs
- [ ] Add explicitly managed shapes whose properties you can change at runtime
- [ ] Include entity index from data pointer in Chipmunk error messages
- [ ] Check(enforce?) proper deallocation of bodies/shapes/constraints on overwrites etc.
- [ ] Spatial queries
- [ ] Fix issues on [travis](https://travis-ci.org/jonascarpay/phycs)
- [ ] Hackage
- [ ] Presolve/Postsolve collision handlers
- [ ] Remaining collision properties
- [ ] Wildcard collision handlers
- [ ] Remaining body/shape/space/constraint/etc. query functions
- [ ] Haddocks
- [ ] Deallocate C bodies/shapes/spaces in SpacePtr cleanup
- [ ] Proper convex polygon rendering
- [ ] Clean up pragmas
- [ ] Move examples to separate package
- [ ] Benchmarks
- [ ] Performance tuning (inlining etc.)

### Want
- [ ] Move rendering to separate package?
- [ ] Figure out how to solve orphan instances
- [ ] More convenience shapes
- [ ] Consistent variable naming
- [ ] Tests
- [ ] Render Constraints
- [ ] `playWorld` camera tracking?
- [ ] Documentation/tutorials?
- [ ] Proper conversion between C structs and data types, instead of field-by-field copying
- [ ] Switch to normal FFI instead of inline-c

### Safety
- [ ] Cannot set mass of non-dynamic body
- [ ] Cannot simulate when mass/moment = 0, probably unenforcable
- [ ] Cannot create/delete objects from collision callbacks
