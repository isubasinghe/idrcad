# OpenSCAD example coverage

`idrcad` covers all 50 `.scad` files under OpenSCAD's `examples/` directory at
revision [`fa8ff891`](https://github.com/openscad/openscad/tree/fa8ff8916a9090d9bc64e9d3ad2725ba1aa74dce/examples).
The accompanying DXF, STL, image, and height-map inputs are kept under
`assets/openscad` with the upstream CC0 dedication.

The ports preserve the teaching purpose of each example. They are intentionally
not transliterations: OpenSCAD modules receiving `children()` become typed Idris
functions, comprehensions become lists, recursion is total, Customizer values
become bounded parameters, and assertions may become solver constraints.

| Upstream source | `idrcad` example |
| --- | --- |
| `Advanced/GEB.scad` | `advanced-geb` |
| `Advanced/animation.scad` | `advanced-animation` |
| `Advanced/assert.scad` | `advanced-assert` |
| `Advanced/children.scad` | `advanced-children` |
| `Advanced/children_indexed.scad` | `advanced-children-indexed` |
| `Advanced/module_recursion.scad` | `advanced-module-recursion` |
| `Advanced/offset.scad` | `advanced-offset` |
| `Advanced/surface_image.scad` | `advanced-surface-image` |
| `Basics/CSG.scad` | `csg` |
| `Basics/CSG-modules.scad` | `csg-modules` |
| `Basics/LetterBlock.scad` | `letter-block` |
| `Basics/hull.scad` | `hull` |
| `Basics/linear_extrude.scad` | `linear-extrude` |
| `Basics/logo.scad` | `logo` |
| `Basics/logo_and_text.scad` | `logo-and-text` |
| `Basics/projection.scad` | `projection` |
| `Basics/roof.scad` | `roof` |
| `Basics/rotate_extrude.scad` | `rotate-extrude` |
| `Basics/text_on_cube.scad` | `text-on-cube` |
| `Functions/echo.scad` | `functions-echo` |
| `Functions/functions.scad` | `functions-functions` |
| `Functions/list_comprehensions.scad` | `functions-list-comprehensions` |
| `Functions/polygon_areas.scad` | `functions-polygon-areas` |
| `Functions/recursion.scad` | `functions-recursion` |
| `Old/example001.scad` | `old-example001` |
| `Old/example002.scad` | `old-example002` |
| `Old/example003.scad` | `old-example003` |
| `Old/example004.scad` | `old-example004` |
| `Old/example005.scad` | `old-example005` |
| `Old/example006.scad` | `old-example006` |
| `Old/example007.scad` | `old-example007` |
| `Old/example008.scad` | `old-example008` |
| `Old/example009.scad` | `old-example009` |
| `Old/example010.scad` | `old-example010` |
| `Old/example011.scad` | `old-example011` |
| `Old/example012.scad` | `old-example012` |
| `Old/example013.scad` | `old-example013` |
| `Old/example014.scad` | `old-example014` |
| `Old/example015.scad` | `old-example015` |
| `Old/example016.scad` | `old-example016` |
| `Old/example017.scad` | `old-example017` |
| `Old/example018.scad` | `old-example018` |
| `Old/example019.scad` | `old-example019` |
| `Old/example020.scad` | `old-example020` |
| `Old/example021.scad` | `old-example021` |
| `Old/example022.scad` | `old-example022` |
| `Old/example023.scad` | `old-example023` |
| `Old/example024.scad` | `old-example024` |
| `Parametric/candleStand.scad` | `parametric-candle-stand` |
| `Parametric/sign.scad` | `parametric-sign` |

Generate any port with:

```sh
./build/exec/idrcad EXAMPLE > model.scad
```

Use `--solve EXAMPLE` for models containing free parameters and `--list` for
the complete command-name list.
