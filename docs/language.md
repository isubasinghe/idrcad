# The `.idrcad` language

The textual language is a small, deterministic frontend to idrcad's typed
Idris model. It intentionally has one spelling for each operation, explicit
units, and no implicit general-purpose arithmetic. That makes it predictable
for people, formatters, and generated output from LLMs.

## Commands

```sh
idrcad check model.idrcad
idrcad build model.idrcad > model.scad
idrcad minizinc model.idrcad
```

`check` parses and semantically elaborates the source without invoking a
solver. `build` also solves the model, validates the complete solution, and
emits OpenSCAD. `minizinc` prints the exact integer constraint model.

## Measurements and dimensions

Every physical literal currently uses millimetres:

```idrcad
3mm
0.25mm
70mm..150mm
```

Decimals are converted directly to integer ticks and may have at most six
fractional digits. A range creates a bounded solver variable; a single value
is fixed. Floating-point values never enter the constraint model.

Geometry translations and sizes use `mm`, rotations use `deg`, and scale
factors are unitless:

```idrcad
placed = move part by [-20mm, 0mm, 4.5mm]
turned = rotate placed by [0deg, 90deg, 0deg]
smaller = scale turned by [0.5, 0.5, 1]
```

## Constructive geometry

Geometry is a sequence of named declarations followed by one `solid` output:

```idrcad
cube = box(width = 15mm, depth = 15mm, height = 15mm, center = true)
ball = sphere(radius = 10mm)
joined = union [cube, ball]
carved = difference cube by [ball]
solid carved
```

The frontend tracks dimensions. `rectangle`, `circle`, `polygon`, `text`,
`regular_polygon`, `star`, and `import2d` produce 2D shapes. `box`,
`rounded_box`, `sphere`, `cylinder`, `polyhedron`, `surface`, and `import3d`
produce 3D shapes. The following operations preserve their input dimension:

- `union`, `difference`, `intersection`, and `hull`
- `move2`, `rotate2`, and `scale2` for 2D geometry
- `move`, `rotate`, and `scale` for 3D geometry
- `colour`, `highlight`, `background`, and `facets`

Dimension-changing operations are explicit:

```idrcad
profile = rectangle(width = 20mm, depth = 10mm, center = true)
solid_profile = extrude profile (height = 4mm, center = false)
twisted = twist_extrude profile (
  height = 30mm, center = false, twist = 90deg, scale = 0.5, slices = 24
)
shadow = projection twisted cut false
revolved = revolve profile (angle = 180deg, convexity = 10)
```

`offset` transforms 2D geometry and `roof` converts a 2D outline to a solid.
`ring` places a 3D child around the Z axis, while `branching_tree` constructs
a level-bounded recursive 2D shape. See `examples/idrcad` for all forms.

Static design requirements use the same constraint IR as solver-derived
relationships:

```idrcad
require 10mm at_least 8mm because "wall thickness must be at least 8 mm"
```

## Declarations

Declarations are processed from top to bottom, and names must be unique.

```idrcad
panel = plate(width = 70mm..150mm, depth = 50mm..100mm, height = 3mm)

display = cutout(
  width = 52mm,
  depth = 30mm,
  clearance = 0.25mm
) in panel

encoder = bore(radius = 4mm) in panel
screws = corner_bores(radius = 1.6mm, edge = 5mm) in panel
```

Cutouts and bores automatically become subtraction features of their parent
plate. Their omitted coordinates become anonymous bounded solver variables.
`corner_bores` is structural: it creates four derived holes without adding
eight position variables.

A constraint-first plate model emits its plate automatically. A constructive
geometry model uses `solid name` to select its output. Both paths elaborate to
the same dimension-indexed geometry tree.

## Relationships

```idrcad
center display in panel
align_x usb with display
align_y encoder with display

usb below display by 8mm
encoder right_of display by 12mm
display left_of screws.lower_right by 4mm

between_columns display screws.lower_left screws.lower_right by 4mm
between_rows display screws.lower_left screws.upper_left by 4mm

space [display, usb, encoder, screws] by 4mm
minimize panel
```

Directional gaps are edge-to-edge. `space` accepts individual features and
patterns; a corner pattern expands to all four holes. A relation requiring one
feature must select a pattern member:

- `lower_left`
- `lower_right`
- `upper_left`
- `upper_right`

`space` lowers to a native two-dimensional non-overlap constraint, allowing
the finite-domain solver to choose the separating axis for each pair.

## Diagnostics and LLM use

Tokens and AST names retain their original source ranges. Syntax errors,
unknown references, duplicate declarations, invalid parent kinds, ambiguous
pattern use, and invalid ranges therefore point to the relevant source token.

For generated models, use this repair loop:

1. Generate a `.idrcad` file using only the documented constructs.
2. Run `idrcad check`.
3. Repair the indicated source range and retry.
4. Run `idrcad build` only after checking succeeds.

The grammar is deliberately closed: do not invent synonyms such as `under`,
`horizontal_center`, or unitless numbers. Comments begin with `#`.
