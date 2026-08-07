# `.idrcad` examples

This directory mirrors the 50 `.scad` models in OpenSCAD's upstream
`examples` tree. These are authored `.idrcad` models, not aliases for hidden
Idris implementations or embedded OpenSCAD. Open one and the declarations you
see are the geometry that gets type checked and emitted.

Start with the CSG example:

```idrcad
model basics_csg

cube = box(width = 15mm, depth = 15mm, height = 15mm, center = true)
ball = sphere(radius = 10mm)
joined = union [cube, ball]
carved = difference cube by [ball]
result = union [joined, carved]
solid result
```

Declarations are processed from top to bottom. CSG and transforms refer to
previously named shapes, and `solid` selects the one 3D output. IdrisCAD tracks
whether each declaration is 2D or 3D, so illegal operations such as extruding
a sphere or subtracting a solid from a polygon are rejected before OpenSCAD.

Useful tours:

- `Basics/CSG.idrcad` — primitives and boolean operations
- `Basics/linear_extrude.idrcad` — checked 2D-to-3D transitions
- `Basics/text_on_cube.idrcad` — text, face placement, and engraving
- `Advanced/assert.idrcad` — requirements become model constraints
- `Advanced/module_recursion.idrcad` — bounded recursive geometry
- `Parametric/` and `Native/front-panel.idrcad` — solver-derived layouts

The ports preserve the lesson of each upstream example, but may use a terser
typed operation or constraint formulation instead of line-for-line OpenSCAD.

Build any example from the repository root:

```sh
./build/exec/idrcad check examples/idrcad/Basics/CSG.idrcad
./build/exec/idrcad build examples/idrcad/Basics/CSG.idrcad > csg.scad

./build/exec/idrcad build examples/idrcad/Native/front-panel.idrcad \
  > front-panel.scad
```

Run `make idrcad-examples` to parse and elaborate the entire directory.

The upstream examples and copied data assets are CC0; see
`assets/openscad/COPYING-CC0.txt`.
