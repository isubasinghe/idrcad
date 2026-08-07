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

The first language version emits one root plate per file. The Idris API can
still express arbitrary geometry trees and multiple solids.

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
