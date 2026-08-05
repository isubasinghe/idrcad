# idrcad

An experimental constraint-derived CAD language in Idris 2. Models are built
as a typed Idris AST and compiled to OpenSCAD source.

The first compatibility target is every `.scad` model in OpenSCAD's
[`examples/Basics`](https://github.com/openscad/openscad/tree/fa8ff8916a9090d9bc64e9d3ad2725ba1aa74dce/examples/Basics)
directory at revision `fa8ff8916a9090d9bc64e9d3ad2725ba1aa74dce`.

## Get started

Enter the reproducible development shell. This provides Idris 2 and
`idris2-lsp`:

```sh
nix develop "path:$PWD"
```

Build and generate the default clearance-fit example:

```sh
make build
./build/exec/idrcad constrained-fit > constrained-fit.scad
openscad constrained-fit.scad
```

List or generate the ported OpenSCAD examples:

```sh
make list
./build/exec/idrcad logo > logo.scad
./build/exec/idrcad rotate-extrude > rotate-extrude.scad
```

Run the Idris tests:

```sh
make test
```

A standalone worked example lives in
[`examples/constrained-fitting`](examples/constrained-fitting). It derives a
pin radius from a toleranced hole and minimum-clearance requirement.

[`examples/partial-fit`](examples/partial-fit) demonstrates a partial setup:
a rectangular plate and centered hole are derived structurally while only the
matching pin radius is exposed to MiniZinc as a decision variable.

## Architecture

- `Idrcad.Expr` defines symbolic scalar arithmetic and evaluation.
- `Idrcad.Fixed` stores exact values as arbitrary-precision integer ticks.
- `Idrcad.Constraint` defines equality and ordering relations, runtime
  validation, and dependent proofs for clearance and toleranced fits.
- `Idrcad.Geometry` defines a dimension-indexed geometry AST.
- `Idrcad.Model` combines parameters, constraints, and geometry.
- `Idrcad.DSL` provides the natural authoring layer over those core types.
- `Idrcad.Backend.OpenSCAD` renders models and constraints as `.scad` source.
- `Idrcad.Backend.MiniZinc` lowers the integer-linear constraint fragment to
  a solver-independent `.mzn` model.
- `Idrcad.Solver.MiniZinc` invokes MiniZinc, parses its integer solution, and
  rejects it unless Idris independently revalidates every bound and constraint.
- `Idrcad.Examples.Basics` contains the upstream Basics ports and the
  clearance-fit example.

`Shape` is indexed by dimension:

```idris
Shape TwoD
Shape ThreeD
```

Operations preserve or explicitly change that index. For example,
`Difference` requires all its operands to have the same dimension,
`LinearExtrude` converts `Shape TwoD` into `Shape ThreeD`, and `Projection`
does the reverse. Dimensionally invalid geometry therefore fails during Idris
type checking.

## Natural DSL

Import `Idrcad.DSL` to get exact units, symbolic arithmetic, relation
operators, the model builder, and the common geometry vocabulary in one place:

```idris
import Idrcad.DSL

fitting = design "Centered hole with fitted cylinder" $ do
  let plate = rectangular 40 30 5
  hole <- exact (mm 10) `at` centreOf plate `through` plate

  pin <- fittedCylinder
    "pin"
    hole
    (mm 5 `within` (mm 1 `to` mm 10))
    16
    (allowing (microns 100) (microns 100) (microns 100))

  maximize pin.cylinderRadius

  solid $ facets 96 $ union
    [ colour "steelblue" (drilled plate hole)
    , colour "orange" pin.cylinderShape
    ]
```

`centreOf plate` derives `(x/2, y/2, z/2)`. The hole registers containment
constraints against all four rectangle edges. `fittedCylinder` inherits the
hole's exact axis by construction, adds the worst-case radial fit constraint,
and leaves only its radius for MiniZinc. This makes coincidence a typed feature
relationship rather than three more coordinates to keep synchronized.

The builder declares each free parameter and returns its `Expr` at the same
time, so names and declarations are not repeated. Idris numeric literals in an
expression mean exact whole units; `mm`, `microns`, and `nanometres` construct
exact parameter values. Ordinary `+`, `-`, and `*` build the symbolic AST.
Relations use `.==.`, `.<.`, `.<=.`, `.>.`, and `.>=.`.

Common geometry reads similarly:

```idris
plate = colour "steelblue" $
  centeredBox plateWidth plateDepth plateThickness `cut`
    [centeredCylinder holeRadius (plateThickness + 2)]

pin = colour "orange" $
  move3 (half plateWidth + 15) 0 0 $
    centeredCylinder pinRadius pinLength

solid $ facets 96 $ union [plate, pin]
```

The DSL is only a surface layer: it produces the same dimension-indexed
`Shape`, `Constraint`, and `Model` values as the explicit constructors. A
constraint can still be checked against an `Environment` in Idris and is also
emitted as an OpenSCAD assertion:

```scad
assert(pin_radius + pin_tolerance + minimum_clearance + hole_tolerance
       <= hole_radius);
```

For exact compile-time dimensions, `ClearanceFit` and `TolerancedFit` carry
erased `LTE` proofs.

## Exact solver arithmetic

The model contains no `Double` values. `Fixed` stores every scalar as an Idris
`Integer`, with one whole CAD unit represented by `1,000,000` ticks. The
authoring DSL gives those ticks physical names:

```idris
mm          10  -- 10.000000 mm
microns    100  --  0.100000 mm
nanometres   1  --  0.000001 mm
```

Addition and subtraction are exact. Multiplication and division return
`Nothing` unless their result is exactly representable at this resolution.
Decimal text is produced only while rendering OpenSCAD.

Parameters also carry explicit lower and upper integer bounds. The MiniZinc
backend accepts only the CP-SAT-friendly fragment: addition, subtraction,
negation, comparisons, and multiplication by whole constants. It rejects
division, fractional coefficients, and products of unknown values.

Solve and render in one command:

```sh
make build
./build/exec/idrcad --solve partial-fit > partial-fit.scad
openscad partial-fit.scad
```

`idrcad` sends the generated model to MiniZinc over stdin, reads the solution,
then independently checks parameter completeness, bounds, and every constraint
in Idris before substituting the values into OpenSCAD. `--minizinc EXAMPLE`
remains available only to inspect the lowered model while debugging.

## Basics coverage

The `idrcad` executable exposes:

```text
constrained-fit
partial-fit
csg
csg-modules
letter-block
hull
linear-extrude
logo
logo-and-text
projection
roof
rotate-extrude
text-on-cube
```

Together these exercise:

- squares, circles, polygons, text, cubes, spheres, cylinders, and 3D imports;
- union, difference, intersection, and hull;
- translation, rotation, scale, color, highlight, and background modifiers;
- linear extrusion, rotate extrusion, projection, and roof;
- fragment, minimum-angle, and minimum-size resolution controls.

The `projection` model references the upstream `projection.stl`, which must be
placed beside its generated `.scad` file. The `roof` primitive requires an
OpenSCAD version that supports `roof()`; OpenSCAD 2021.01 predates it.

OpenSCAD modules are represented as ordinary Idris functions. This retains
reuse and parameterisation while keeping generated OpenSCAD deliberately
simple.
