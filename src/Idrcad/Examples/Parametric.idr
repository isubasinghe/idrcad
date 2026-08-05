module Idrcad.Examples.Parametric

import Idrcad.Examples.Util

%default total

spokes : Nat -> Expr -> Expr -> Expr -> Shape ThreeD
spokes count radius width height = Union $ map spoke (indices count)
  where
    spoke : Nat -> Shape ThreeD
    spoke index = Rotate3D (MkVec3 0 0 (angleAt index count)) $
      Translate3D (MkVec3 (half radius) 0 (half height)) $
        Cube (MkVec3 radius width height) True

holder : Expr -> Expr -> Expr -> Shape ThreeD
holder outerRadius holeRadius height = Difference
  (Cylinder height outerRadius outerRadius False)
  [Translate3D (MkVec3 0 0 (-1)) $
    Cylinder (height + 2) holeRadius holeRadius False]

||| Unlike the Customizer-only upstream model, these dimensions are bounded
||| solver parameters and their fit is an executable constraint.
public export
candleStandModel : Model ThreeD
candleStandModel = design "OpenSCAD Parametric: constrained candle stand" $ do
  standHeight <- parameter "stand_height" $
    mm 50 `within` (mm 30 `to` mm 70)
  ringRadius <- parameter "ring_radius" $
    mm 25 `within` (mm 15 `to` mm 40)
  holderRadius <- parameter "holder_radius" $
    mm 4 `within` (mm 4 `to` mm 7)
  candleRadius <- parameter "candle_radius" $
    mm 3 `within` (mm 2 `to` mm 5)
  clearance <- tolerance "candle_clearance" (microns 250) (mm 1)

  assert (candleRadius + clearance .<=. holderRadius)
    "Candle and radial clearance must fit each holder"
  assert (3 * holderRadius .<=. ringRadius)
    "Ring radius must leave conservative spacing between holders"

  let count : Nat = 7
      holderHeight = 7
      supportHeight = 3
      top = standHeight - half holderHeight
      cup = holder holderRadius candleRadius holderHeight
      outerHolders = Translate3D (MkVec3 0 0 top) $
        ring3D ringRadius count cup
      centerHolder = Translate3D (MkVec3 0 0 top) cup
      supportRing = Translate3D (MkVec3 0 0 top) $
        extrude 4 $ Difference (Circle ringRadius) [Circle (ringRadius - 2)]

  minimize ringRadius

  solid $ WithResolution (resolution (Just 96) Nothing Nothing) $ Union
    [ Cylinder standHeight 2 2 False
    , spokes count ringRadius 3 supportHeight
    , supportRing
    , outerHolders
    , centerHolder
    ]

public export
signModel : Model ThreeD
signModel = design "OpenSCAD Parametric: sign" $ do
  radius <- parameter "sign_radius" $
    mm 80 `within` (mm 60 `to` mm 200)
  height <- parameter "sign_height" $
    mm 2 `within` (mm 1 `to` mm 10)

  assert (10 .<. radius) "Sign radius must leave room for its recessed face"

  let plaque = Scale3D (MkVec3 1 (f 500000) 1) $ Difference
        (Cylinder (2 * height) radius radius True)
        [Translate3D (MkVec3 0 0 height) $
          Cylinder (height + 1) (radius - 10) (radius - 10) True]
      lettering = extrude height $ Union
        [ Translate2D (v2 0 4) (text2 "Welcome to..." 10)
        , Translate2D (v2 0 (-16)) (text2 "Parametric Designs" 10)
        ]

  solid $ WithResolution (resolution (Just 50) Nothing Nothing) $
    Union [plaque, lettering]

public export
parametric : List (String, Model ThreeD)
parametric =
  [ ("parametric-candle-stand", candleStandModel)
  , ("parametric-sign", signModel)
  ]
