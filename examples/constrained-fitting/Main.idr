module Main

import Data.Nat
import Idrcad.Backend.OpenSCAD
import Idrcad.DSL

%default total

||| Constructive evidence that n <= n + extra.
ltePlusRight : (n : Nat) -> (extra : Nat) -> LTE n (n + extra)
ltePlusRight Z extra = LTEZero
ltePlusRight (S n) extra = LTESucc (ltePlusRight n extra)

||| A compile-time fit measured in tenths of a millimetre:
||| 9.7 mm pin + 0.1 mm pin tolerance + 0.1 mm clearance
||| + 0.1 mm hole tolerance <= 10 mm nominal hole radius.
staticFit : TolerancedFit 97 1 100 1 1
staticFit = MkTolerancedFit (ltePlusRight 100 0)

fittingModel : Model ThreeD
fittingModel = design "Derived toleranced fitting" $ do
  holeRadius <- parameter "hole_radius" $
    mm 10 `within` (mm 1 `to` mm 1000)
  holeTolerance <- tolerance "hole_tolerance" (microns 100) (mm 100)
  pinTolerance <- tolerance "pin_tolerance" (microns 100) (mm 100)
  clearance <- tolerance "minimum_clearance" (microns 100) (mm 100)
  plateWidth <- parameter "plate_width" $
    mm 50 `within` (mm 1 `to` mm 1000)
  plateDepth <- parameter "plate_depth" $
    mm 35 `within` (mm 1 `to` mm 1000)
  plateThickness <- parameter "plate_thickness" $
    mm 5 `within` (mm 1 `to` mm 1000)
  pinLength <- parameter "pin_length" $
    mm 16 `within` (mm 1 `to` mm 1000)

  -- The mating part is derived directly from the fit requirements.
  let pinRadius = holeRadius - holeTolerance - pinTolerance - clearance
      worstCaseFit = pinRadius + pinTolerance + clearance + holeTolerance

  assert (worstCaseFit .<=. holeRadius)
    "Worst-case pin must fit inside the minimum hole radius"
  positive "Derived pin radius" pinRadius
  assert (2 * holeRadius .<=. plateWidth)
    "Hole must fit inside the plate width"
  assert (2 * holeRadius .<=. plateDepth)
    "Hole must fit inside the plate depth"

  let plate = colour "steelblue" $
        centeredBox plateWidth plateDepth plateThickness `cut`
          [centeredCylinder holeRadius (plateThickness + 2)]
      pin = colour "orange" $
        move3 (half plateWidth + 15) 0 0 $
          centeredCylinder pinRadius pinLength

  solid $ facets 96 $ union [plate, pin]

covering
main : IO ()
main = putStr (renderModel fittingModel)
