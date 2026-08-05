module Idrcad.Examples.PartialFit

import Idrcad.DSL

%default total

||| A relationship-first assembly:
|||   * the plate is a known x-by-y-by-z rectangle;
|||   * the hole lies at its derived center (x/2, y/2, z/2);
|||   * the pin inherits that exact axis;
|||   * MiniZinc chooses the largest radius satisfying the radial fit.
public export
partialFitModel : Model ThreeD
partialFitModel = design "Centered hole with a solver-fitted cylinder" $ do
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
