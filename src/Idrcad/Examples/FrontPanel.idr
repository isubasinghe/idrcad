module Idrcad.Examples.FrontPanel

import Idrcad.DSL

%default total

||| A solver-laid-out electronics front panel. Only relationships are fixed:
||| the display is centred, USB is below it, and the encoder is to its right.
||| Corner screws and manufacturing clearances force the final panel size.
public export
frontPanelModel : Model ThreeD
frontPanelModel = design "Solver-laid-out electronics front panel" $ do
  panelWidth <- parameter "panel_width" $
    mm 117 `within` (mm 70 `to` mm 150)
  panelDepth <- parameter "panel_depth" $
    mm 86 `within` (mm 50 `to` mm 100)
  displayX <- parameter "display_x" $
    microns 58500 `within` (mm 30 `to` mm 75)
  displayY <- parameter "display_y" $
    mm 43 `within` (mm 25 `to` mm 50)
  encoderX <- parameter "encoder_x" $
    microns 100750 `within` (mm 50 `to` mm 145)
  usbY <- parameter "usb_y" $
    mm 16 `within` (mm 10 `to` mm 50)

  let thickness = exact (mm 3)
      panel = rectangular panelWidth panelDepth thickness
      panelMidplane = half thickness

      screwRadius = exact (microns 1600)
      edgeClearance = exact (mm 5)
      screwInset = screwRadius + edgeClearance
      featureSpacing = exact (mm 4)

      lowerLeft = point3 screwInset screwInset panelMidplane
      lowerRight = point3 (panelWidth - screwInset) screwInset panelMidplane
      upperLeft = point3 screwInset (panelDepth - screwInset) panelMidplane
      upperRight = point3
        (panelWidth - screwInset)
        (panelDepth - screwInset)
        panelMidplane

  -- Centering is a linear equality, so no division enters MiniZinc.
  assert (2 * displayX .==. panelWidth)
    "Display must be horizontally centred"
  assert (2 * displayY .==. panelDepth)
    "Display must be vertically centred"

  display <- rectangularCutout
    "Display"
    (mm 52)
    (mm 30)
    (microns 250)
    (point3 displayX displayY panelMidplane)
    panel

  usb <- rectangularCutout
    "USB"
    (mm 13)
    (mm 7)
    (microns 200)
    (point3 displayX usbY panelMidplane)
    panel

  encoder <- exact (mm 4)
    `at` point3 encoderX displayY panelMidplane
    `through` panel

  lowerLeftScrew <- screwRadius `at` lowerLeft `through` panel
  lowerRightScrew <- screwRadius `at` lowerRight `through` panel
  upperLeftScrew <- screwRadius `at` upperLeft `through` panel
  upperRightScrew <- screwRadius `at` upperRight `through` panel

  -- The encoder is right of the display, with finger clearance between them.
  assert
    (displayX + display.cutoutHalfWidth + 12 + encoder.holeRadius
      .<=. encoderX)
    "Encoder must sit at least 12 mm to the right of the display"

  -- Keep the encoder away from the right-hand screw column.
  assert
    (encoderX + encoder.holeRadius + featureSpacing
      .<=. lowerRightScrew.holeAxis.pointX - screwRadius)
    "Encoder must clear the right mounting screws"

  -- USB is centered under the display, with an 8 mm edge-to-edge gap.
  assert
    (usbY + usb.cutoutHalfDepth + 8
      .<=. displayY - display.cutoutHalfDepth)
    "USB socket must sit at least 8 mm below the display"

  -- The lower screws constrain how far down the USB socket may move.
  assert
    (lowerLeftScrew.holeAxis.pointY + screwRadius + featureSpacing
      .<=. usbY - usb.cutoutHalfDepth)
    "USB socket must clear the lower mounting screws"

  -- The display stays between both screw columns and rows.
  assert
    (lowerLeftScrew.holeAxis.pointX + screwRadius + featureSpacing
      .<=. displayX - display.cutoutHalfWidth)
    "Display must clear the left mounting screws"
  assert
    (displayX + display.cutoutHalfWidth + featureSpacing
      .<=. lowerRightScrew.holeAxis.pointX - screwRadius)
    "Display must clear the right mounting screws"
  assert
    (lowerLeftScrew.holeAxis.pointY + screwRadius + featureSpacing
      .<=. displayY - display.cutoutHalfDepth)
    "Display must clear the lower mounting screws"
  assert
    (displayY + display.cutoutHalfDepth + featureSpacing
      .<=. upperLeftScrew.holeAxis.pointY - screwRadius)
    "Display must clear the upper mounting screws"

  minimize (panelWidth + panelDepth)

  let holes =
        [ display.cutoutShape
        , usb.cutoutShape
        , encoder.holeShape
        , lowerLeftScrew.holeShape
        , lowerRightScrew.holeShape
        , upperLeftScrew.holeShape
        , upperRightScrew.holeShape
        ]
      panelShape = colour "steelblue" (cutFeatures panel holes)

      -- Component bodies make the solved layout easy to read in OpenSCAD.
      displayBody = colour "black" $
        move3 displayX displayY (thickness + 1) $
          centeredBox 52 30 2
      encoderKnob = colour "orange" $
        move3 encoderX displayY thickness $
          Cylinder 10 8 8 False
      usbBody = colour "silver" $
        move3 displayX usbY (thickness + 2) $
          centeredBox 13 7 4
      screwHead : Point3D -> Shape ThreeD
      screwHead (Point x y z) = colour "silver" $
        move3 x y (thickness + 1) $
          centeredCylinder 3 2

  solid $ facets 64 $ union
    [ panelShape
    , displayBody
    , encoderKnob
    , usbBody
    , screwHead lowerLeft
    , screwHead lowerRight
    , screwHead upperLeft
    , screwHead upperRight
    ]
