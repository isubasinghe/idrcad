# Solver-laid-out front panel

This example starts with component requirements instead of a finished panel:

- a `52 x 30 mm` display with `0.25 mm` clearance per side;
- a `13 x 7 mm` USB socket with `0.2 mm` clearance per side;
- a 4 mm-radius encoder opening;
- four 1.6 mm-radius mounting holes, each 5 mm from the panel edge;
- explicit display-to-encoder, display-to-USB, and screw clearances.

The display must be centered, the encoder must be to its right, and USB must
be below it. Their final coordinates and the panel dimensions are not written
into the geometry. MiniZinc finds the smallest feasible `width + depth`, then
Idris checks the complete integer solution before generating OpenSCAD.

From the repository root:

```sh
nix develop "path:$PWD"
make front-panel
./examples/front-panel/build/exec/front-panel > front-panel.scad
openscad front-panel.scad
```

The optimal result is a `116.9 x 85.7 x 3 mm` panel:

```text
display centre = (58.45, 42.85)
encoder centre = (100.7, 42.85)
USB centre     = (58.45, 15.9)
```

Use `--defaults` to inspect the authored starting values, or `--minizinc` to
inspect the integer constraint model. Normal use invokes MiniZinc itself.
