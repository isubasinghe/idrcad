# Partial fit example

This model starts with a manually sized `40 x 30 x 5` rectangular plate. A
10 mm radius hole is placed at `centreOf plate`, which derives the symbolic
point `(x/2, y/2, z/2)` and registers edge-containment constraints. The
matching cylinder inherits that hole axis, so its position cannot drift away
from the hole.

Only `pin_radius` remains free. Its authored default is intentionally 5 mm,
while `maximize` asks MiniZinc for the largest radius satisfying clearance and
both manufacturing tolerances: 9.7 mm.

From the repository root:

```sh
nix develop "path:$PWD"
make partial-fit

# This automatically invokes MiniZinc, validates its answer in Idris,
# and emits solved OpenSCAD.
./examples/partial-fit/build/exec/partial-fit > partial-fit.scad
openscad partial-fit.scad
```

The central feature relationships are:

```idris
let plate = rectangular 40 30 5
hole <- exact (mm 10) `at` centreOf plate `through` plate

pin <- fittedCylinder
  "pin"
  hole
  (mm 5 `within` (mm 1 `to` mm 10))
  16
  (allowing (microns 100) (microns 100) (microns 100))

maximize pin.cylinderRadius
```

Use `--defaults` to bypass solving or `--minizinc` to inspect the generated
solver model. Normal use requires neither option.
