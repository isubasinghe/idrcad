module Idrcad.Examples.FrontPanel

import Idrcad.DSL

%default total

||| A solver-laid-out electronics front panel. Sizes, anonymous positions,
||| structural corner patterns, and spatial relationships are declared here;
||| MiniZinc chooses the smallest feasible result.
public export
frontPanelModel : Model ThreeD
frontPanelModel = design "Solver-laid-out electronics front panel" $ do
  panel <- plate
    (starting 117 (between 70 150))
    (starting 86 (between 50 100))
    (exactly 3)

  display <- centeredCutout panel $
    rect 52 30 `withClearance` (microns 250)
  usb <- cutoutIn panel $
    rect 13 7 `withClearance` (microns 200)
  encoder <- boreIn panel (mm 4)
  screws <- cornerBores panel (mm 5) (microns 1600)

  alignX usb display
  usb `below` display $ 8

  alignY encoder display
  encoder `rightOf` display $ 12

  encoder `leftOf` screws.lowerRightBore $ 4
  usb `above` screws.lowerLeftBore $ 4
  betweenColumns
    display screws.lowerLeftBore screws.lowerRightBore 4
  betweenRows
    display screws.lowerLeftBore screws.upperLeftBore 4

  -- This single global lets the finite-domain solver select a separating axis
  -- for every pair; the directional statements above express design intent.
  spaced (mm 4)
    [ footprint display
    , footprint usb
    , footprint encoder
    , footprint screws.lowerLeftBore
    , footprint screws.lowerRightBore
    , footprint screws.upperLeftBore
    , footprint screws.upperRightBore
    ]

  minimumPlate panel

  let Point displayX displayY displayZ = display.cutoutCentre
      Point usbX usbY usbZ = usb.cutoutCentre
      Point encoderX encoderY encoderZ = encoder.holeAxis
      thickness = panel.rectangleHeight
      holes =
        [ display.cutoutShape
        , usb.cutoutShape
        , encoder.holeShape
        , screws.lowerLeftBore.holeShape
        , screws.lowerRightBore.holeShape
        , screws.upperLeftBore.holeShape
        , screws.upperRightBore.holeShape
        ]
      panelShape = colour "steelblue" (cutFeatures panel holes)
      displayBody = colour "black" $
        move3 displayX displayY (thickness + 1) $
          centeredBox 52 30 2
      encoderKnob = colour "orange" $
        move3 encoderX encoderY thickness $
          Cylinder 10 8 8 False
      usbBody = colour "silver" $
        move3 usbX usbY (thickness + 2) $
          centeredBox 13 7 4
      screwHead : Hole -> Shape ThreeD
      screwHead screw =
        let Point x y z = screw.holeAxis
         in colour "silver" $
              move3 x y (thickness + 1) $
                centeredCylinder 3 2

  solid $ facets 64 $ union
    [ panelShape
    , displayBody
    , encoderKnob
    , usbBody
    , screwHead screws.lowerLeftBore
    , screwHead screws.lowerRightBore
    , screwHead screws.upperLeftBore
    , screwHead screws.upperRightBore
    ]
