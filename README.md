# idrcad

Describe what must fit, align, and stay apart. Let the solver determine the
dimensions and positions, then emit ordinary OpenSCAD.

![A solver-laid-out electronics front panel](docs/front-panel.png)

`idrcad` is an experimental constraint-derived CAD language in Idris 2. It
combines a dimension-checked geometry tree, exact fixed-point measurements,
MiniZinc constraint solving, and OpenSCAD as a practical rendering backend.

## Why idrcad?

- Express relationships such as *inside*, *centred*, *right of*, and *fits
  with clearance* instead of manually synchronising coordinates.
- Leave sizes and positions partially specified and have MiniZinc complete the
  design or optimize it.
- Model tolerances without floating-point arithmetic: all solver values are
  arbitrary-precision integer ticks.
- Catch 2D/3D geometry mistakes through Idris types.
- Recheck every solver result in Idris before it reaches generated geometry.

For example, a hole and its matching pin share an axis by construction while
MiniZinc chooses the largest safe pin radius:

```idris
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

  solid $ union
    [ drilled plate hole
    , pin.cylinderShape
    ]
```

## Try the front-panel solver

The front panel in the screenshot has no hand-named coordinates or layout
equations. Features expose typed 2D footprints, so the same words work for
cutouts, bores, and structural patterns:

```idris
panel <- plate (between 70 150) (between 50 100) (exactly 3)
display <- centeredCutout panel $
  rect 52 30 `withClearance` (microns 250)
usb <- cutoutIn panel $ rect 13 7 `withClearance` (microns 200)
encoder <- boreIn panel (mm 4)
screws <- cornerBores panel (mm 5) (microns 1600)

alignX usb display
usb `below` display $ 8
alignY encoder display
encoder `rightOf` display $ 12

spaced (mm 4) [footprint display, footprint usb, footprint encoder]
minimumPlate panel
```

The complete example adds the screw relationships; MiniZinc derives a
`116.9 × 85.7 × 3 mm` panel and every anonymous component position.

```sh
nix develop "path:$PWD"
make build
./build/exec/idrcad --solve front-panel > front-panel.scad
openscad front-panel.scad
```

The command invokes MiniZinc itself, parses the integer solution, validates all
bounds and constraints in Idris, and substitutes the checked values into the
OpenSCAD output.

## Exact constraints

There are no `Double` values in the constraint model. One CAD unit is stored
as `1,000,000` integer ticks:

```idris
mm          10  -- 10.000000 mm
microns    100  --  0.100000 mm
nanometres   1  --  0.000001 mm
```

The MiniZinc backend accepts the integer-linear fragment plus a native 2D
non-overlap global: comparisons, addition, subtraction, negation, and
multiplication by whole constants. It rejects fractional coefficients,
division, and products of unknown values.

## Examples

- [`examples/front-panel`](examples/front-panel): solver-driven component
  layout and minimum panel dimensions.
- [`examples/partial-fit`](examples/partial-fit): a known plate and hole with
  a solver-sized matching cylinder.
- [`examples/constrained-fitting`](examples/constrained-fitting): a toleranced
  clearance fit.
- The [coverage map](docs/examples.md) links runnable Idris ports to all 50
  files in OpenSCAD's `Advanced`, `Basics`, `Functions`, `Old`, and
  `Parametric` example groups at the pinned upstream revision.

List everything or run the tests with:

```sh
make list
make test
```

The Nix development shell includes Idris 2, `idris2-lsp`, and MiniZinc.
`--minizinc EXAMPLE` prints the generated solver model when you want to inspect
what the DSL lowered.
