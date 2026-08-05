module Idrcad.Examples.Functions

import Idrcad.Examples.Util

%default total

resultLine : Integer -> String -> String -> Shape ThreeD
resultLine row name value = Translate3D (v3 0 (-10 * row) 0) $
  text3 (name ++ " = " ++ value) 7 1

||| OpenSCAD's echo example has no geometry. The Idris port makes the pure
||| function results visible while ordinary Idris tracing remains available
||| during development.
public export
echoModel : Model ThreeD
echoModel = MkModel "OpenSCAD Functions: echo results" [] [] Satisfy $
  named "cyan" $ Union
    [ resultLine 0 "f1(3,5)" "25.5"
    , resultLine 1 "f2(4)" "64"
    , resultLine 2 "f3(5)" "20"
    , resultLine 3 "f4(5)" "120"
    ]

samples : Integer -> Integer -> Nat -> List Expr
samples start step count = map sample (indices count)
  where
    sample : Nat -> Expr
    sample index = e start + e step * natExpr index

linearFunction : Expr -> Expr
linearFunction x = half x + 1

vectorFunction : Expr -> Vec3
vectorFunction x =
  let fx = linearFunction x
   in MkVec3 (5 * x + 20) (fx * fx - 50) 0

public export
functionsModel : Model ThreeD
functionsModel = MkModel "OpenSCAD Functions: functions" [] [] Satisfy $
  Union
    [ named "red" $ Union $ map
        (\a => Translate3D (MkVec3 a (linearFunction a) 0) (cubeOf 2 True))
        (samples (-100) 5 41)
    , named "green" $ Union $ map
        (\a => Translate3D (vectorFunction (a `dividedBy` 8)) (Sphere 1))
        (samples (-200) 10 41)
    ]

isEven : Nat -> Bool
isEven Z = True
isEven (S Z) = False
isEven (S (S value)) = isEven value

starPoint : Nat -> Expr -> Expr -> Nat -> Vec2
starPoint count inner outer index =
  let angle = angleAt index count
      radius = if isEven index then inner else outer
   in MkVec2 (radius * cosine angle) (radius * sine angle)

star : Nat -> Expr -> Expr -> Shape TwoD
star count inner outer = Polygon $
  map (starPoint count inner outer) (indices count)

roundedPolygon : Nat -> Expr -> Expr -> Shape TwoD
roundedPolygon sides radius rounding =
  Offset2D (RadialOffset rounding) (regularPolygon sides (radius - rounding))

public export
listComprehensionsModel : Model ThreeD
listComprehensionsModel = MkModel "OpenSCAD Functions: list comprehensions" [] [] Satisfy $
  thin $ Union
    [ regularPolygon 3 10
    , Translate2D (v2 20 0) (regularPolygon 6 8)
    , Translate2D (v2 36 0) (regularPolygon 10 6)
    , Translate2D (v2 0 22) (roundedPolygon 3 10 5)
    , Translate2D (v2 20 22) (roundedPolygon 6 8 4)
    , Translate2D (v2 36 22) (roundedPolygon 10 6 3)
    , Translate2D (v2 0 44) (star 20 6 10)
    , Translate2D (v2 20 44) (star 40 6 8)
    , Translate2D (v2 36 44) (star 30 3 6)
    ]

shapeWithArea : Expr -> Nat -> String -> Shape ThreeD
shapeWithArea x sides areaText = Union
  [ thin $ Translate2D (MkVec2 x 0) (regularPolygon sides 10)
  , Translate3D (MkVec3 x (-20) 0) $ named "cyan" $
      text3 areaText 8 1
  ]

public export
polygonAreasModel : Model ThreeD
polygonAreasModel = MkModel "OpenSCAD Functions: polygon areas" [] [] Satisfy $
  Union
    [ Translate3D (v3 0 20 0) $ named "red" $ text3 "Areas:" 8 1
    , shapeWithArea (-44) 3 "130"
    , shapeWithArea (-22) 4 "200"
    , shapeWithArea 0 6 "260"
    , shapeWithArea 22 10 "294"
    , shapeWithArea 44 360 "314"
    ]

factorial : Nat -> Integer
factorial Z = 1
factorial value@(S predecessor) =
  factorial predecessor * factorialFactor value
  where
    factorialFactor : Nat -> Integer
    factorialFactor Z = 0
    factorialFactor (S rest) = 1 + factorialFactor rest

public export
recursionModel : Model ThreeD
recursionModel = MkModel "OpenSCAD Functions: total recursion" [] [] Satisfy $
  named "cyan" $ text3 ("6! = " ++ show (factorial 6)) 10 1

public export
functionExamples : List (String, Model ThreeD)
functionExamples =
  [ ("functions-echo", echoModel)
  , ("functions-functions", functionsModel)
  , ("functions-list-comprehensions", listComprehensionsModel)
  , ("functions-polygon-areas", polygonAreasModel)
  , ("functions-recursion", recursionModel)
  ]
