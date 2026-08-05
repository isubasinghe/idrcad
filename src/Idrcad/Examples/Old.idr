module Idrcad.Examples.Old

import Idrcad.Examples.Util

%default total

oldModel : String -> Shape ThreeD -> Model ThreeD
oldModel number = MkModel ("OpenSCAD Old: example" ++ number) [] [] Satisfy

conicalHole : Vec3 -> Shape ThreeD
conicalHole rotation = Rotate3D rotation $
  Cylinder (f 62500000) (f 12500000) (f 6250000) True

public export
old001Model : Model ThreeD
old001Model = oldModel "001" $ Difference (Sphere 25)
  [ Cylinder (f 62500000) (f 12500000) (f 12500000) True
  , Rotate3D (v3 90 0 0) $
      Cylinder (f 62500000) (f 12500000) (f 12500000) True
  , Rotate3D (v3 0 90 0) $
      Cylinder (f 62500000) (f 12500000) (f 12500000) True
  ]

crossNegative : Shape ThreeD
crossNegative = Union
  [ Cube (v3 50 10 10) True
  , Cube (v3 10 50 10) True
  , Cube (v3 10 10 50) True
  ]

public export
old002Model : Model ThreeD
old002Model = oldModel "002" $ Intersection
  [ Difference
      (Union
        [ Cube (v3 30 30 30) True
        , Translate3D (v3 0 0 (-25)) (Cube (v3 15 15 50) True)
        ])
      [crossNegative]
  , Translate3D (v3 0 0 5) $
      Cylinder 50 20 5 True
  ]

public export
old003Model : Model ThreeD
old003Model = oldModel "003" $ Difference
  (Union
    [ Cube (v3 30 30 30) True
    , Cube (v3 40 15 15) True
    , Cube (v3 15 40 15) True
    , Cube (v3 15 15 40) True
    ])
  [crossNegative]

public export
old004Model : Model ThreeD
old004Model = oldModel "004" $ Difference (cubeOf 30 True) [Sphere 20]

public export
old005Model : Model ThreeD
old005Model = oldModel "005" $ Translate3D (v3 0 0 (-120)) $ Union
  [ Difference (cylinderOf 50 100 False)
      [ Translate3D (v3 0 0 10) (cylinderOf 50 80 False)
      , Translate3D (v3 100 0 35) (cubeOf 50 True)
      ]
  , ring3D 80 6 (cylinderOf 200 10 False)
  , Translate3D (v3 0 0 200) (Cylinder 80 120 0 False)
  ]

roundedCube : Expr -> Expr -> Shape ThreeD
roundedCube size radius =
  let edge = half size - radius
      corners =
        [ MkVec3 (-edge) (-edge) (-edge), MkVec3 (-edge) (-edge) edge
        , MkVec3 (-edge) edge (-edge), MkVec3 (-edge) edge edge
        , MkVec3 edge (-edge) (-edge), MkVec3 edge (-edge) edge
        , MkVec3 edge edge (-edge), MkVec3 edge edge edge
        ]
   in Hull $ map (\point => Translate3D point (Sphere radius)) corners

pip : Expr -> Expr -> Shape ThreeD
pip x z = Translate3D (MkVec3 x (-50) z) (Sphere 10)

public export
old006Model : Model ThreeD
old006Model = oldModel "006" $ WithResolution
  (resolution (Just 48) Nothing Nothing) $
  Difference (roundedCube 100 10)
    [ pip 0 0
    , Rotate3D (v3 0 0 90) $ Union [pip (-20) (-20), pip 20 20]
    , Rotate3D (v3 0 0 180) $ Union
        [pip (-20) (-25), pip (-20) 0, pip (-20) 25,
         pip 20 (-25), pip 20 0, pip 20 25]
    , Rotate3D (v3 0 0 270) $ Union
        [pip 0 0, pip (-25) (-25), pip 25 (-25), pip (-25) 25, pip 25 25]
    , Rotate3D (v3 90 0 0) $ Union [pip (-25) (-25), pip 0 0, pip 25 25]
    , Rotate3D (v3 (-90) 0 0) $ Union
        [pip (-25) (-25), pip 25 (-25), pip (-25) 25, pip 25 25]
    ]

dxf : String -> String -> Shape TwoD
dxf file layer = Import2D ("assets/openscad/Old/" ++ file) (Just layer)

public export
old007Model : Model ThreeD
old007Model =
  let profile = dxf "example007.dxf" "dorn"
      clip = RotateExtrude (MkRotateExtrudeOptions Nothing (Just 3)) profile
      cutout = Intersection
        [ Rotate3D (v3 90 0 0) $ Translate3D (v3 0 0 (-50)) $
            extrude 100 (dxf "example007.dxf" "cutout1")
        , Rotate3D (v3 90 0 90) $ Translate3D (v3 0 0 (-50)) $
            extrude 100 (dxf "example007.dxf" "cutout2")
        ]
   in oldModel "007" $ Translate3D (v3 0 0 (-10)) $
        Difference clip [cutout, Rotate3D (v3 0 0 90) cutout]

public export
old008Model : Model ThreeD
old008Model =
  let letter : String -> Shape ThreeD
      letter layer = extrude 50 (dxf "example008.dxf" layer)
      word = Intersection
        [ Translate3D (v3 (-25) (-25) (-25)) (letter "G")
        , Rotate3D (v3 90 0 0) $
            Translate3D (v3 (-25) (-125) (-25)) (letter "E")
        , Rotate3D (v3 0 90 0) $
            Translate3D (v3 (-125) (-125) (-25)) (letter "B")
        ]
      crossCut = Intersection
        [ Translate3D (v3 (-125) (-25) (-26)) $ extrude 52 (dxf "example008.dxf" "X")
        , Rotate3D (v3 0 90 0) $ Translate3D (v3 (-125) (-25) (-26)) $
            extrude 52 (dxf "example008.dxf" "X")
        ]
   in oldModel "008" (Difference word [crossCut])

public export
old009Model : Model ThreeD
old009Model =
  let bodyWidth = 50
      fanWidth = 30
      plateWidth = 3
      body = Background $ centeredExtrude bodyWidth (dxf "example009.dxf" "body")
      plates = Background $ Union
        [ Translate3D (v3 0 0 27) $ centeredExtrude plateWidth (dxf "example009.dxf" "plate")
        , Translate3D (v3 0 0 (-27)) $ centeredExtrude plateWidth (dxf "example009.dxf" "plate")
        ]
      fan = Intersection
        [ LinearExtrude (MkLinearExtrudeOptions fanWidth True (Just (-35)) Nothing (Just 10)) $
            dxf "example009.dxf" "fan_top"
        , RotateExtrude (MkRotateExtrudeOptions Nothing (Just 10)) $
            Translate2D (v2 0 (-40)) (dxf "example009.dxf" "fan_side")
        ]
   in oldModel "009" (Union [body, plates, fan])

public export
old010Model : Model ThreeD
old010Model = oldModel "010" $ Intersection
  [ Surface "assets/openscad/Old/example010.dat" True
  , Rotate3D (v3 0 0 45) $ Surface "assets/openscad/Old/example010.dat" True
  ]

public export
old011Model : Model ThreeD
old011Model = oldModel "011" $ Polyhedron
  [ v3 10 0 0, v3 0 10 0, v3 (-10) 0 0, v3 0 (-10) 0, v3 0 0 10 ]
  [ [0, 1, 2, 3], [4, 1, 0], [4, 2, 1], [4, 3, 2], [4, 0, 3] ]

public export
old012Model : Model ThreeD
old012Model = oldModel "012" $ Difference (Sphere 20)
  [ Translate3D (v3f (-2920000) 500000 20000000) $
      Rotate3D (v3 180 0 180) $
        Import3D "assets/openscad/Old/example012.stl"
  ]

public export
old013Model : Model ThreeD
old013Model =
  let source = centeredExtrude 100 (Import2D
        "assets/openscad/Old/example013.dxf" Nothing)
   in oldModel "013" $ Intersection
        [ source
        , Rotate3D (v3 0 90 0) source
        , Rotate3D (v3 90 0 0) source
        ]

public export
old014Model : Model ThreeD
old014Model = oldModel "014" $ Intersection
  [ Cube (v3 100 20 20) True
  , Rotate3D (v3 10 20 300) (Cube (v3 100 20 20) True)
  , Rotate3D (v3 200 40 57) (Cube (v3 100 20 20) True)
  , Rotate3D (v3 20 88 57) (Cube (v3 100 20 20) True)
  ]

public export
old015Model : Model ThreeD
old015Model =
  let frame = Difference (Square (uniform2 100) True) [Square (uniform2 50) True]
      band = Rotate2D 45 $ Translate2D (v2 0 (-15)) $
        Square (v2 100 30) False
      native = Difference
        (Translate2D (v2 (-35) (-35)) $ Intersection
          [Union [frame, Translate2D (v2 50 50) (Square (uniform2 15) True)], band])
        [Rotate2D (-45) $ Scale2D (MkVec2 (f 700000) (f 1300000)) (Circle 5)]
      imported = Scale2D (uniform2 2) (dxf "example009.dxf" "body")
   in oldModel "015" $ thin (Union [native, imported])

public export
old016Model : Model ThreeD
old016Model =
  let block = Cube (v3 65 28 28) True
      blade = Difference
        (Translate3D (v3f 0 0 7500000) (Cube (v3 60 28 14) True))
        [cubeOf 8 True]
      chop = Translate3D (v3 (-18) 0 0) $
        Import3D "assets/openscad/Old/example016.stl"
      cut = Difference blade [chop]
   in oldModel "016" $ Difference block
        [ cut, Rotate3D (v3 90 0 0) cut,
          Rotate3D (v3 180 0 0) cut, Rotate3D (v3 270 0 0) cut ]

public export
old017Model : Model ThreeD
old017Model =
  let innerDisc = extrude 6 $ Difference (Circle 37) [Circle 25]
      outerDisc = extrude 6 $ Difference (Circle 106) [Circle 75]
      tripod = Translate3D (v3 0 80 40) $ Cube (v3 6 80 6) True
      braces = named "lightblue" $ Union
        [tripod, Rotate3D (v3 0 0 120) tripod, Rotate3D (v3 0 0 240) tripod]
      bottle = Background $ Translate3D (v3 0 0 12) (Cylinder 68 25 18 False)
   in oldModel "017" $ Union
        [ outerDisc
        , Translate3D (v3 0 0 74) innerDisc
        , braces
        , bottle
        ]

rotateOnce : List value -> List value
rotateOnce [] = []
rotateOnce (value :: rest) = rest ++ [value]

rotateList : Nat -> List value -> List value
rotateList Z values = values
rotateList (S count) values = rotateList count (rotateOnce values)

childRow : Nat -> Shape ThreeD
childRow row =
  let children =
        [ Sphere 30
        , cubeOf 60 True
        , cylinderOf 50 30 True
        , Union [cubeOf 45 True,
            Rotate3D (v3 45 0 0) (cubeOf 50 True),
            Rotate3D (v3 0 45 0) (cubeOf 50 True),
            Rotate3D (v3 0 0 45) (cubeOf 50 True)]
        ]
      place : (Nat, Shape ThreeD) -> Shape ThreeD
      place (index, child) = Translate3D
        (MkVec3 (100 * (natExpr index - f 1500000)) (100 * natExpr row - 250) 0)
        child
   in Union $ map place $ enumerate $ rotateList row children

public export
old018Model : Model ThreeD
old018Model = oldModel "018" $ Union (map childRow [1, 2, 3, 4])

lookupHeight : Integer -> Integer
lookupHeight position =
  if position <= (-50) then 20 + ((position + 50) `div` 10)
  else if position <= (-20) then 20 - ((position + 50) `div` 15)
  else if position <= 80 then 18 + ((position + 20) `div` 14)
  else 25 - ((position - 80) `div` 3)

integerSamples : Integer -> Integer -> Nat -> List Integer
integerSamples start step count = map
  (\index => start + step * natToInteger index)
  (indices count)
  where
    natToInteger : Nat -> Integer
    natToInteger Z = 0
    natToInteger (S value) = 1 + natToInteger value

public export
old019Model : Model ThreeD
old019Model = oldModel "019" $ Union $ map column (integerSamples (-100) 5 41)
  where
    column : Integer -> Shape ThreeD
    column position = Translate3D (v3 position 0 (-30)) $
      Cylinder (e (3 * lookupHeight position)) 6 2 False

threadProfile : Shape TwoD
threadProfile = Difference (Circle 20) [ring2D 20 7 (Circle 5)]

screw : Expr -> Expr -> Shape ThreeD
screw height twists = LinearExtrude
  (MkLinearExtrudeOptions height False (Just twists) Nothing (Just 8))
  threadProfile

public export
old020Model : Model ThreeD
old020Model =
  let bolt = Translate3D (v3 (-30) 0 0) (screw 100 (f 411428571))
      nut = Translate3D (v3 30 0 0) $ Difference
        (WithResolution (resolution (Just 6) Nothing Nothing) $ cylinderOf 20 30 False)
        [Translate3D (v3 0 0 (-10)) (screw 40 (f 164571429))]
      spring = LinearExtrude
        (MkLinearExtrudeOptions 100 False (Just 180) Nothing (Just 8)) $
        Translate2D (v2 100 0) (Circle 10)
   in oldModel "020" $ Union [bolt, nut, spring]

projectedThing : Shape ThreeD
projectedThing = Difference (Sphere 25)
  [ conicalHole (v3 0 0 0)
  , conicalHole (v3 90 0 0)
  , conicalHole (v3 0 90 0)
  ]

projectionSlice : Integer -> Shape ThreeD
projectionSlice height = Rotate3D (v3 (-30) (-30) 0) $
  Translate3D (v3 0 0 (-height)) $
    centeredExtrude (f 500000) $ Projection True $
      Translate3D (v3 0 0 height) $
        Rotate3D (v3 30 30 0) projectedThing

public export
old021Model : Model ThreeD
old021Model = oldModel "021" $ Union
  [ Translate3D (v3 (-30) 0 0) $ Union
      [centeredExtrude (f 500000) (Projection False projectedThing), Background projectedThing]
  , Translate3D (v3 30 0 0) $ Union
      (Background projectedThing :: map projectionSlice [-20, -15, -10, -5, 0, 5, 10, 15, 20])
  ]

roundedBox : Expr -> Expr -> Expr -> Expr -> Shape ThreeD
roundedBox width depth height radius =
  let x = half width - radius
      y = half depth - radius
      z = half height - radius
      corners =
        [MkVec3 (-x) (-y) (-z), MkVec3 (-x) (-y) z,
         MkVec3 (-x) y (-z), MkVec3 (-x) y z,
         MkVec3 x (-y) (-z), MkVec3 x (-y) z,
         MkVec3 x y (-z), MkVec3 x y z]
   in Hull $ map (\point => Translate3D point (Sphere radius)) corners

public export
old022Model : Model ThreeD
old022Model = oldModel "022" $ Union
  [ Translate3D (v3 (-15) 0 0) $ Hull
      [Cube (v3 10 30 40) True, Cube (v3 20 20 40) True]
  , Translate3D (v3 15 0 0) (roundedBox 20 30 40 5)
  ]

clockWords : List String
clockWords =
  ["one", "two", "three", "four", "five", "six",
   "seven", "eight", "nine", "ten", "eleven", "twelve"]

clockWord : (Nat, String) -> Shape ThreeD
clockWord (index, word) = Rotate3D (MkVec3 0 0 (90 - angleAt (S index) 12)) $
  Translate3D (v3 16 0 0) (text3 word 4 2)

public export
old023Model : Model ThreeD
old023Model = oldModel "023" $ Union (map clockWord (enumerate clockWords))

negativeOffsets : List (Expr, Expr)
negativeOffsets =
  [((-1), (-1)), ((-1), 0), ((-1), 1),
   (0, (-1)), (0, 1),
   (1, (-1)), (1, 0), (1, 1)]

mengerNegative : Nat -> Expr -> Expr -> Shape ThreeD
mengerNegative Z side maximum = Cube (MkVec3 (f 1100000 * maximum) side side) True
mengerNegative (S level) side maximum =
  let third = side `dividedBy` 3
      child : (Expr, Expr) -> Shape ThreeD
      child (y, z) = Translate3D (MkVec3 0 (y * third) (z * third)) $
        mengerNegative level third maximum
   in Union (Cube (MkVec3 (f 1100000 * maximum) third third) True
        :: map child negativeOffsets)

public export
old024Model : Model ThreeD
old024Model =
  let sponge = Difference (cubeOf 100 True)
        [ mengerNegative 2 100 100
        , Rotate3D (v3 0 0 90) (mengerNegative 2 100 100)
        , Rotate3D (v3 0 90 0) (mengerNegative 2 100 100)
        ]
      posed = Rotate3D (MkVec3 45 (arcTangent2 1 (squareRoot 2)) 0) sponge
   in oldModel "024" $ Difference posed
        [Translate3D (v3 0 0 (-100)) (cubeOf 200 True)]

public export
old : List (String, Model ThreeD)
old =
  [ ("old-example001", old001Model), ("old-example002", old002Model)
  , ("old-example003", old003Model), ("old-example004", old004Model)
  , ("old-example005", old005Model), ("old-example006", old006Model)
  , ("old-example007", old007Model), ("old-example008", old008Model)
  , ("old-example009", old009Model), ("old-example010", old010Model)
  , ("old-example011", old011Model), ("old-example012", old012Model)
  , ("old-example013", old013Model), ("old-example014", old014Model)
  , ("old-example015", old015Model), ("old-example016", old016Model)
  , ("old-example017", old017Model), ("old-example018", old018Model)
  , ("old-example019", old019Model), ("old-example020", old020Model)
  , ("old-example021", old021Model), ("old-example022", old022Model)
  , ("old-example023", old023Model), ("old-example024", old024Model)
  ]
