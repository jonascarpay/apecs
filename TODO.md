This is how I keep track of the issues I still need to fix.
If you want to help, I think it's easiest if you create an issue for the one you are fixing and assigning yourself, but I'm open to ideas.

### Need
- [x] Make separate example executables
- [x] Constraints
- [ ] Collision callbacks
- [x] Add Gloss `play` version to allow interactive games
- [ ] Implement remaining global properties
- [ ] Read Constraints/Shapes
- [ ] Add explicitly managed shapes whose properties you can change at runtime
- [ ] Include entity index from data pointer in Chipmunk error messages
- [ ] Check(enforce?) proper deallocation of bodies/shapes/constraints on overwrites etc.
- [ ] Spatial queries
- [ ] Fix issues on [travis](https://travis-ci.org/jonascarpay/phycs)
- [ ] Hackage
- [ ] Remaining body/shape/space/constraint/etc. query functions
- [ ] Haddocks
- [ ] Deallocate C bodies/shapes/spaces in SpacePtr cleanup
- [ ] Proper convex polygon rendering
- [ ] Clean up pragmas

### Want
- [ ] Split out rendering into a separate module
- [ ] Figure out how to solve orphan instances
- [ ] Benchmarks
- [ ] `playWorld` camera tracking?
