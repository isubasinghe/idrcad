module Idrcad.Examples.Basics

import Idrcad.Constraint
import Idrcad.DSL
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import Idrcad.Examples.PartialFit

e : Integer -> Expr
e = integer

f : Integer -> Expr
f value = Lit (millionths value)

v2 : Integer -> Integer -> Vec2
v2 x y = MkVec2 (e x) (e y)

v2f : Integer -> Integer -> Vec2
v2f x y = MkVec2 (f x) (f y)

v3 : Integer -> Integer -> Integer -> Vec3
v3 x y z = MkVec3 (e x) (e y) (e z)

v3f : Integer -> Integer -> Integer -> Vec3
v3f x y z = MkVec3 (f x) (f y) (f z)

cubeOf : Integer -> Bool -> Shape ThreeD
cubeOf size = Cube (uniform3 (e size))

sphereOf : Integer -> Shape ThreeD
sphereOf radius = Sphere (e radius)

cylinderOf : Integer -> Integer -> Bool -> Shape ThreeD
cylinderOf height radius = Cylinder (e height) (e radius) (e radius)

named : String -> Shape dimension -> Shape dimension
named name = Colourise (NamedColour name)

resolution : Maybe Nat -> Maybe Fixed -> Maybe Fixed -> Resolution
resolution fragments angle size =
  MkResolution fragments (map Lit angle) (map Lit size)

public export
csgModel : Model ThreeD
csgModel = MkModel "OpenSCAD Basics: CSG" [] [] Satisfy $
  Union
    [ Translate3D (v3 (-24) 0 0) $
        Union [cubeOf 15 True, sphereOf 10]
    , Intersection [cubeOf 15 True, sphereOf 10]
    , Translate3D (v3 24 0 0) $
        Difference (cubeOf 15 True) [sphereOf 10]
    ]

body : Shape ThreeD
body = named "Blue" (sphereOf 10)

intersector : Shape ThreeD
intersector = named "Red" (cubeOf 15 True)

holeObject : Shape ThreeD
holeObject = named "Lime" (cylinderOf 20 5 True)

holeA : Shape ThreeD
holeA = Rotate3D (v3 0 90 0) holeObject

holeB : Shape ThreeD
holeB = Rotate3D (v3 90 0 0) holeObject

holeC : Shape ThreeD
holeC = holeObject

holes : Shape ThreeD
holes = Union [holeA, holeB, holeC]

intersected : Shape ThreeD
intersected = Intersection [body, intersector]

helperLine : Shape ThreeD
helperLine = named "Black" (cylinderOf 10 1 True)

helpers : Shape ThreeD
helpers = Scale3D (uniform3 (f 500000)) $
  Union
    [ Translate3D (v3 (-30) 0 (-40)) $
        Union
          [ intersected
          , Translate3D (v3 (-15) 0 (-35)) body
          , Translate3D (v3 15 0 (-35)) intersector
          , Translate3D (v3f (-7500000) 0 (-17500000)) $
              Rotate3D (v3 0 30 0) helperLine
          , Translate3D (v3f 7500000 0 (-17500000)) $
              Rotate3D (v3 0 (-30) 0) helperLine
          ]
    , Translate3D (v3 30 0 (-40)) $
        Union
          [ holes
          , Translate3D (v3 (-10) 0 (-35)) holeA
          , Translate3D (v3 10 0 (-35)) holeB
          , Translate3D (v3 30 0 (-35)) holeC
          , Translate3D (v3f 5000000 0 (-17500000)) $
              Rotate3D (v3 0 (-20) 0) helperLine
          , Translate3D (v3f (-5000000) 0 (-17500000)) $
              Rotate3D (v3 0 30 0) helperLine
          , Translate3D (v3f 15000000 0 (-17500000)) $
              Rotate3D (v3 0 (-45) 0) helperLine
          ]
    , Translate3D (v3f (-20000000) 0 (-22500000)) $
        Rotate3D (v3 0 45 0) helperLine
    , Translate3D (v3f 20000000 0 (-22500000)) $
        Rotate3D (v3 0 (-45) 0) helperLine
    ]

public export
csgModulesModel : Model ThreeD
csgModulesModel = MkModel "OpenSCAD Basics: CSG modules" [] [] Satisfy $
  WithResolution (resolution Nothing (Just (whole 5)) (Just (tenths 1))) $
    Union
      [ Difference (Intersection [body, intersector]) [holes]
      , helpers
      ]

letterBlock : String -> Expr -> Shape ThreeD
letterBlock letter size =
  let textOptions = MkTextOptions
        letter
        (divide (multiply size (e 22)) (e 30))
        (Just "Liberation Sans:style=bold")
        AlignCenter
        AlignCenterV
        (e 1)
      engraving = LinearExtrude
        (MkLinearExtrudeOptions size False Nothing Nothing (Just 4))
        (Text2D textOptions)
   in Difference
        (Translate3D (MkVec3 (e 0) (e 0) (divide size (e 4))) $
          Cube (MkVec3 size size (divide size (e 2))) True)
        [ Translate3D
            (MkVec3 (e 0) (e 0) (divide size (e 6)))
            engraving
        ]

public export
letterBlockModel : Model ThreeD
letterBlockModel = MkModel "OpenSCAD Basics: LetterBlock" [] [] Satisfy $
  letterBlock "Y" (e 30)

public export
hullModel : Model ThreeD
hullModel = MkModel "OpenSCAD Basics: hull" [] [] Satisfy $
  WithResolution (resolution Nothing Nothing (Just (tenths 1))) $
    Union
      [ Hull
          [ Translate3D (v3f 0 30000000 2500000) (sphereOf 1)
          , Translate3D (v3 0 10 (-1)) $
              Cube (v3 20 1 8) True
          , Translate3D (v3 0 10 (-5)) $
              Cube (v3 1 1 8) True
          , Translate3D (v3 0 (-8) (-1)) $
              Cube (v3 20 1 8) True
          , Translate3D (v3 0 (-8) (-5)) $
              Cube (v3 1 1 8) True
          , Translate3D (v3 0 (-30) 1) $
              Cube (v3 16 1 4) True
          ]
      , cylinderOf 40 1 False
      ]

extrudedRectangle : LinearExtrudeOptions -> Shape ThreeD
extrudedRectangle options =
  LinearExtrude options (Square (v2 20 10) True)

public export
linearExtrudeModel : Model ThreeD
linearExtrudeModel = MkModel "OpenSCAD Basics: linear_extrude" [] [] Satisfy $
  Union
    [ named "red" $
        Translate3D (v3 0 (-30) 0) $
          extrudedRectangle (defaultLinearExtrude (e 20))
    , named "green" $
        Translate3D (v3 (-30) 0 0) $
          extrudedRectangle $
            MkLinearExtrudeOptions
              (e 20) False Nothing (Just (UniformExtrude (f 200000))) Nothing
    , named "cyan" $
        Translate3D (v3 30 0 0) $
          extrudedRectangle $
            MkLinearExtrudeOptions
              (e 20) False (Just (e 90)) Nothing Nothing
    , named "gray" $
        Translate3D (v3 0 30 0) $
          WithResolution (resolution Nothing (Just (whole 1)) (Just (whole 1))) $
            extrudedRectangle $
              MkLinearExtrudeOptions
                (e 40)
                True
                (Just (e (-360)))
                (Just (UniformExtrude (e 0)))
                Nothing
    ]

logo : Expr -> Shape ThreeD
logo size =
  let holeRadius = divide size (e 4)
      cylinderHeight = multiply size (f 1250000)
      hole = Cylinder cylinderHeight holeRadius holeRadius True
   in WithResolution (resolution (Just 100) Nothing Nothing) $
        Difference
          (Sphere (divide size (e 2)))
          [ hole
          , Highlight (Rotate3D (v3 90 0 0) hole)
          , Rotate3D (v3 0 90 0) hole
          ]

public export
logoModel : Model ThreeD
logoModel = MkModel "OpenSCAD Basics: logo" [] [] Satisfy (logo (e 50))

logoText : String -> Expr -> String -> Expr -> Shape ThreeD
logoText content size style spacing =
  let options = MkTextOptions
        content
        size
        (Just ("Liberation Sans" ++ style))
        AlignLeft
        AlignBaseline
        spacing
   in Rotate3D (v3 90 0 0) $
        LinearExtrude (defaultLinearExtrude (e 1)) $
          WithResolution (resolution (Just 16) Nothing Nothing) $
            Text2D options

rgb255 : Integer -> Integer -> Integer -> Colour
rgb255 red green blue = RGB
  (divide (e red) (e 255))
  (divide (e green) (e 255))
  (divide (e blue) (e 255))

public export
logoAndTextModel : Model ThreeD
logoAndTextModel = MkModel "OpenSCAD Basics: logo and text" [] [] Satisfy $
  Translate3D (v3 110 0 80) $
    Union
      [ Translate3D (v3 0 0 30) $
          Rotate3D (v3 25 25 (-40)) (logo (e 120))
      , Translate3D (v3 100 0 40) $
          Colourise (rgb255 157 203 81) $
            logoText "Open" (e 42) ":style=Bold" (f 1050000)
      , Translate3D (v3 247 0 40) $
          Colourise (rgb255 249 210 44) $
            logoText "SCAD" (e 42) ":style=Bold" (f 900000)
      , Translate3D (v3 100 0 0) $
          named "black" $
            logoText "The Programmers" (e 18) ":style=Bold" (e 1)
      , Translate3D (v3 160 0 (-30)) $
          named "black" $
            logoText "Solid 3D CAD Modeller" (e 18) ":style=Bold" (e 1)
      ]

projectionPanel : Colour -> Vec3 -> Vec3 -> Shape ThreeD -> Shape ThreeD
projectionPanel colour translation rotation source =
  Colourise colour $
    Rotate3D rotation $
      Translate3D translation $
        LinearExtrude
          (MkLinearExtrudeOptions (e 2) True Nothing Nothing Nothing) $
          Difference
            (Square (uniform2 (e 30)) True)
            [Projection False source]

public export
projectionModel : Model ThreeD
projectionModel =
  let source = Import3D "projection.stl"
   in MkModel "OpenSCAD Basics: projection" [] [] Satisfy $
        Union
          [ Background source
          , projectionPanel
              (NamedColour "red")
              (v3 0 0 (-20))
              (v3 0 0 0)
              source
          , projectionPanel
              (NamedColour "green")
              (v3 0 0 (-20))
              (v3 0 90 0)
              (Rotate3D (v3 0 90 0) source)
          , projectionPanel
              (NamedColour "cyan")
              (v3 0 0 20)
              (v3 (-90) 0 0)
              (Rotate3D (v3 90 0 0) source)
          , Colourise (RGBA (e 1) (e 1) (e 0) (f 500000)) $
              Translate3D (v3 0 0 20) $
                LinearExtrude
                  (MkLinearExtrudeOptions (e 2) True Nothing Nothing Nothing) $
                  Difference
                    (Square (uniform2 (e 30)) True)
                    [Projection True source]
          ]

roofSketch : Shape TwoD
roofSketch = Polygon
  [ v2 (-5) (-1)
  , v2f (-150000) (-1000000)
  , v2 0 0
  , v2f 150000 (-1000000)
  , v2 5 (-1)
  , v2f 5000000 (-100000)
  , v2 4 0
  , v2f 5000000 100000
  , v2 5 1
  , v2 (-5) 1
  ]

roofText : String -> Shape TwoD
roofText content = Text2D $
  MkTextOptions content (e 2) Nothing AlignLeft AlignCenterV (e 1)

public export
roofModel : Model ThreeD
roofModel = MkModel "OpenSCAD Basics: roof" [] [] Satisfy $
  Union
    [ Roof StraightSkeleton roofSketch
    , Translate3D (v3 0 (-5) 0) (Roof Voronoi roofSketch)
    , Translate3D (v3 0 (-8) 0) $
        WithResolution (resolution (Just 4) Nothing Nothing) $
          Roof Voronoi roofSketch
    , Translate3D (v3 6 0 0) $
        Roof StraightSkeleton (roofText "straight skeleton")
    , Translate3D (v3 6 (-7) 0) $
        Roof Voronoi (roofText "Voronoi diagram")
    ]

squareProfile : Shape TwoD
squareProfile = Square (uniform2 (e 5)) False

partialRevolve : Fixed -> Integer -> Shape ThreeD
partialRevolve offset angle =
  RotateExtrude (MkRotateExtrudeOptions (Just (e angle)) Nothing) $
    Translate2D (MkVec2 (Lit offset) (e 0)) squareProfile

public export
rotateExtrudeModel : Model ThreeD
rotateExtrudeModel = MkModel "OpenSCAD Basics: rotate_extrude" [] [] Satisfy $
  Union
    [ named "red" $
        RotateExtrude defaultRotateExtrude $
          Translate2D (v2 10 0) squareProfile
    , named "cyan" $
        Translate3D (v3 40 0 0) $
          WithResolution (resolution (Just 80) Nothing Nothing) $
            RotateExtrude defaultRotateExtrude $
              Text2D (defaultText "  J" (e 10))
    , named "green" $
        Translate3D (v3 0 30 0) $
          WithResolution (resolution (Just 80) Nothing Nothing) $
            RotateExtrude defaultRotateExtrude $
              Polygon
                [ v2 0 0
                , v2 8 4
                , v2 4 8
                , v2 4 12
                , v2 12 16
                , v2 0 20
                ]
    , named "magenta" $
        Translate3D (v3 40 40 0) $
          Union
            [ partialRevolve (tenths 125) 180
            , Translate3D (v3f 7500000 0 0) (partialRevolve (whole 5) 180)
            , Translate3D (v3f (-7500000) 0 0) (partialRevolve (whole 5) (-180))
            ]
    ]

cubeLetter : Expr -> Expr -> String -> Shape ThreeD
cubeLetter letterSize letterHeight content =
  LinearExtrude (defaultLinearExtrude letterHeight) $
    WithResolution (resolution (Just 16) Nothing Nothing) $
      Text2D $
        MkTextOptions
          content
          letterSize
          (Just "Liberation Sans")
          AlignCenter
          AlignCenterV
          (e 1)

public export
textOnCubeModel : Model ThreeD
textOnCubeModel =
  let cubeSize = e 60
      letterSize = e 50
      letterHeight = e 5
      offset = subtract (divide cubeSize (e 2)) (divide letterHeight (e 2))
      letter = cubeLetter letterSize letterHeight
      sides = Union
        [ named "gray" (Cube (uniform3 cubeSize) True)
        , Translate3D (MkVec3 (e 0) (Negate offset) (e 0)) $
            Rotate3D (v3 90 0 0) (letter "C")
        , Translate3D (MkVec3 offset (e 0) (e 0)) $
            Rotate3D (v3 90 0 90) (letter "U")
        , Translate3D (MkVec3 (e 0) offset (e 0)) $
            Rotate3D (v3 90 0 180) (letter "B")
        , Translate3D (MkVec3 (Negate offset) (e 0) (e 0)) $
            Rotate3D (v3 90 0 (-90)) (letter "E")
        ]
   in MkModel "OpenSCAD Basics: text on cube" [] [] Satisfy $
        Difference sides
          [ Translate3D (MkVec3 (e 0) (e 0) offset) (letter "☺")
          , Translate3D
              (MkVec3 (e 0) (e 0) (Negate (add offset letterHeight)))
              (letter "☼")
          ]

public export
constrainedFitModel : Model ThreeD
constrainedFitModel = design "Constraint-derived clearance fit" $ do
  holeRadius <- parameter "hole_radius" $
    mm 10 `within` (mm 1 `to` mm 1000)
  holeTolerance <- tolerance "hole_tolerance" (microns 100) (mm 100)
  pinRadius <- parameter "pin_radius" $
    microns 9500 `within` (mm 1 `to` mm 1000)
  pinTolerance <- tolerance "pin_tolerance" (microns 100) (mm 100)
  clearance <- tolerance "minimum_clearance" (microns 200) (mm 100)
  plateWidth <- parameter "plate_width" $
    mm 40 `within` (mm 1 `to` mm 1000)
  plateHeight <- parameter "plate_height" $
    mm 30 `within` (mm 1 `to` mm 1000)
  plateThickness <- parameter "plate_thickness" $
    mm 4 `within` (mm 1 `to` mm 1000)
  pinLength <- parameter "pin_length" $
    mm 12 `within` (mm 1 `to` mm 1000)

  let holeDiameter = 2 * holeRadius
      worstCaseFit = pinRadius + pinTolerance + clearance + holeTolerance

  assert (worstCaseFit .<=. holeRadius)
    "Worst-case pin radius plus clearance must fit the minimum hole radius"
  assert (holeDiameter .<=. plateWidth)
    "The hole diameter must fit inside the plate width"
  assert (holeDiameter .<=. plateHeight)
    "The hole diameter must fit inside the plate height"
  positive "Pin radius" pinRadius
  positive "Hole radius" holeRadius

  let plate = centeredBox plateWidth plateHeight plateThickness `cut`
        [centeredCylinder holeRadius (plateThickness + 2)]
      pin = move3 (half plateWidth + 10) 0 0 $
        centeredCylinder pinRadius pinLength

  solid $ facets 96 $ union
    [ colour "steelblue" plate
    , colour "orange" pin
    ]

||| Ports of every .scad model in OpenSCAD's examples/Basics directory.
public export
basics : List (String, Model ThreeD)
basics =
  [ ("csg", csgModel)
  , ("csg-modules", csgModulesModel)
  , ("letter-block", letterBlockModel)
  , ("hull", hullModel)
  , ("linear-extrude", linearExtrudeModel)
  , ("logo", logoModel)
  , ("logo-and-text", logoAndTextModel)
  , ("projection", projectionModel)
  , ("roof", roofModel)
  , ("rotate-extrude", rotateExtrudeModel)
  , ("text-on-cube", textOnCubeModel)
  ]

public export
examples : List (String, Model ThreeD)
examples =
  ("constrained-fit", constrainedFitModel)
    :: ("partial-fit", partialFitModel)
    :: basics

public export
findExample : String -> Maybe (Model ThreeD)
findExample requested = findIn examples
  where
    findIn : List (String, Model ThreeD) -> Maybe (Model ThreeD)
    findIn [] = Nothing
    findIn ((name, model) :: rest) =
      if requested == name then Just model else findIn rest
