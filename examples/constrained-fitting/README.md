# Constrained fitting example

This example creates a plate with a toleranced circular hole and derives the
nominal radius of a matching pin from:

```text
pin radius = hole radius
             - hole tolerance
             - pin tolerance
             - minimum clearance
```

The same relationship is represented by a `TolerancedFit` proof at compile
time, checked symbolically in Idris, and emitted as an OpenSCAD `assert()`.
All dimensions are exact fixed-point integers; decimal conversion happens only
when OpenSCAD source is rendered.

It also demonstrates the natural `Idrcad.DSL` surface: bounded parameters are
declared inside a `design` block, arithmetic uses ordinary operators, and
constraints read as `assert (left .<=. right) "message"`.

From the repository root:

```sh
nix develop "path:$PWD"
make constrained-fitting
./examples/constrained-fitting/build/exec/constrained-fitting \
  > constrained-fitting.scad
openscad constrained-fitting.scad
```
