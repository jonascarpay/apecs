# apecs-gloss-3d

Depth-sorted CPU 3D rendering on top of
[gloss](https://hackage.haskell.org/package/gloss) — the 3D sibling of
apecs-gloss, same prototyping-grade ambitions and no physics-engine
dependency.

Gloss has no depth buffer; this package leans on two facts instead:
disjoint bodies sorted by view depth paint in the correct order, and
the front faces of a convex polyhedron never overlap in projection, so
per-body rendering needs only backface culling.

Primitives (`sphere3`, `solid3`, `wire3`, `hiddenWire3`, `blob3`)
return depth-tagged `Piece`s; `assemble` (or `foldDraw3`) sorts them
far-to-near into a `Picture`. A `Camera3` look-at camera and an `Env3`
light/fog environment resolve into a per-frame `Scene3`. Convex
`Solid`s come prebuilt (`boxSolid`, `cubeSolid`) or from arbitrary
face cycles via `facesSolid` (normals computed and auto-oriented);
`solidEdges` extracts a solid's edges for `wire3`. Vectors are
linear's `V3 Float` with `Quaternion Float` rotations; adapt your
engine's types at the call site — see the apecs-box3d demos: the
tumbler rendered with `solid3`/`sphere3`/`wire3`, and the
`apecs-box3d-elite` chase built on `facesSolid` hulls.
