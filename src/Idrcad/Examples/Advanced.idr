module Idrcad.Examples.Advanced

import Idrcad.Examples.Util

%default total

letter : String -> Expr -> Shape TwoD
letter content expansion =
  Offset2D (DeltaOffset expansion False) $
    Text2D (MkTextOptions
      content 10 (Just "Liberation Sans") AlignCenter AlignCenterV 1)

gebSolid : Shape ThreeD
gebSolid = Intersection
  [ centeredExtrude 20 (letter "B" (f 500000))
  , Rotate3D (v3 90 0 0) $ centeredExtrude 20 (letter "E" (f 300000))
  , Rotate3D (v3 90 0 90) $ centeredExtrude 20 (letter "G" (f 300000))
  ]

public export
gebModel : Model ThreeD
gebModel = MkModel "OpenSCAD Advanced: GEB" [] [] Satisfy $
  WithResolution (resolution (Just 64) Nothing Nothing) $
    Union
      [ named "Ivory" gebSolid
      , named "MediumOrchid" $ Translate3D (v3 0 0 (-20)) $
          extrude 1 $ Difference (Square (uniform2 40) True)
            [Projection False gebSolid]
      , named "DarkMagenta" $ Rotate3D (v3 90 0 0) $
          Translate3D (v3 0 0 (-20)) $
            extrude 1 $ Difference (Square (MkVec2 40 39) True)
              [Projection False (Rotate3D (v3 (-90) 0 0) gebSolid)]
      , named "MediumSlateBlue" $ Rotate3D (v3 90 0 90) $
          Translate3D (v3 0 0 (-20)) $
            extrude 1 $ Difference (Square (MkVec2 39 39) True)
              [Projection False (Rotate3D (v3 0 (-90) (-90)) gebSolid)]
      ]

armSegment : String -> Expr -> Shape ThreeD
armSegment colorName length = named colorName $ Hull
  [ Sphere 2
  , Translate3D (MkVec3 length 0 0) (Sphere 2)
  ]

||| A reproducible snapshot of the upstream SCARA animation. OpenSCAD's `$t`
||| is deliberately replaced by an explicit model value.
public export
animationModel : Model ThreeD
animationModel =
  let firstAngle = 35
      secondAngle = 118
      firstLength = 70
      secondLength = 50
      firstArm = Rotate3D (MkVec3 0 0 firstAngle) $
        armSegment "red" firstLength
      secondArm = Rotate3D (MkVec3 0 0 firstAngle) $
        Translate3D (MkVec3 firstLength 0 0) $
          Rotate3D (MkVec3 0 0 (secondAngle - firstAngle)) $
            armSegment "green" secondLength
      joint = Rotate3D (MkVec3 0 0 firstAngle) $
        Translate3D (MkVec3 firstLength 0 0) (Sphere 4)
      plate = Background $ Translate3D (v3 0 25 (-6)) $
        Cube (MkVec3 150 150 (f 100000)) True
   in MkModel "OpenSCAD Advanced: animation snapshot" [] [] Satisfy $
        WithResolution (resolution (Just 30) Nothing Nothing) $
          Union [plate, Sphere 4, Cylinder 12 2 2 True, firstArm, joint, secondArm]

assertedRing : Expr -> Nat -> Expr -> Shape ThreeD
assertedRing radius count size =
  ring3D radius count (Cube (MkVec3 size size size) True)

public export
assertModel : Model ThreeD
assertModel = design "OpenSCAD Advanced: typed assertions" $ do
  assert (10 .>=. 10) "Ring radius must be at least 10 mm"
  assert (3 .>=. 3) "Ring needs at least three members"
  assert (20 .<=. 20) "Ring supports at most twenty members"
  solid $ Union
    [ named "red" (assertedRing 10 3 4)
    , named "green" (assertedRing 25 9 6)
    , named "blue" (assertedRing 40 20 8)
    ]

something : Shape ThreeD
something = Union
  [ cubeOf 10 True
  , WithResolution (resolution (Just 40) Nothing Nothing) $
      cylinderOf 12 2 False
  , Translate3D (v3 0 0 12) $ Rotate3D (v3 90 0 0) $
      centeredExtrude 2 (text2 "SCAD" 8)
  , Translate3D (v3f 0 0 12000000) $
      Cube (v3f 22000000 1600000 400000) True
  ]

public export
childrenModel : Model ThreeD
childrenModel = MkModel "OpenSCAD Advanced: typed child combinators" [] [] Satisfy $
  Union
    [ named "red" $ ring3D 15 6 (cubeOf 8 True)
    , named "green" $ ring3D 30 12 $
        Difference (Sphere 5) [cylinderOf 12 2 True]
    , named "cyan" $ ring3D 50 4 something
    ]

indexedRow : List (Shape ThreeD) -> Shape ThreeD
indexedRow children =
  let count = length children
      centerOffset = half (natExpr (minus count 1))
      place : (Nat, Shape ThreeD) -> Shape ThreeD
      place (index, child) =
        let factor = 1 + natExpr index `dividedBy` natExpr count
            x = 15 * (natExpr index - centerOffset)
         in Translate3D (MkVec3 x (40 + 20 * natExpr index) 0) $
              Scale3D (uniform3 factor) child
   in Union (map place (enumerate children))

indexedGroup : Expr -> String -> List (Shape ThreeD) -> Shape ThreeD
indexedGroup x label children = Translate3D (MkVec3 x (-20) 0) $ named label $
  case children of
    [] => text3 "Nothing..." 6 1
    values => Union [text3 (show (length values) ++ " objects") 6 1, indexedRow values]

public export
childrenIndexedModel : Model ThreeD
childrenIndexedModel = MkModel "OpenSCAD Advanced: indexed typed children" [] [] Satisfy $
  Union
    [ indexedGroup (-100) "red" []
    , indexedGroup (-50) "yellow" [cubeOf 5 True]
    , indexedGroup 0 "cyan" [cubeOf 5 True, Sphere 4]
    , indexedGroup 50 "green"
        [cubeOf 5 True, Sphere 4, cylinderOf 5 4 False]
    ]

tree2D : Nat -> Expr -> Expr -> Shape TwoD
tree2D Z length thickness = Square (MkVec2 thickness length) False
tree2D (S level) length thickness =
  let trunk = Square (MkVec2 thickness length) False
      nextLength = f 700000 * length
      nextThickness = f 800000 * thickness
      branch : Expr -> Shape TwoD
      branch angle = Translate2D (MkVec2 0 length) $
        Rotate2D angle (tree2D level nextLength nextThickness)
   in Union [trunk, branch 28, branch (-31)]

public export
moduleRecursionModel : Model ThreeD
moduleRecursionModel = MkModel "OpenSCAD Advanced: module recursion" [] [] Satisfy $
  named "forestgreen" $ thin (tree2D 8 100 5)

public export
offsetModel : Model ThreeD
offsetModel =
  let base = LinearExtrude
        (MkLinearExtrudeOptions 20 False Nothing
          (Just (UniformExtrude (f 500000))) Nothing) $
        Offset2D (DeltaOffset 10 False) (Square (uniform2 50) True)
      outline = Difference
        (Offset2D (DeltaOffset 1 False) (Circle 15))
        [Offset2D (DeltaOffset (-1) False) (Circle 15)]
   in MkModel "OpenSCAD Advanced: offset" [] [] Satisfy $
        WithResolution (resolution (Just 40) Nothing Nothing) $
          Union
            [ base
            , Translate3D (v3 0 0 20) (extrude 20 outline)
            , Background (cylinderOf 100 14 False)
            , Background (Translate3D (v3 0 0 100) (Sphere 30))
            ]

surfaceLayer : Integer -> String -> Shape ThreeD
surfaceLayer level colorName =
  named colorName $ Translate3D (v3 0 0 (2 * (level - 1))) $
    extrude 2 $ Projection True $
      Translate3D (v3 0 0 (-30 * level)) $
        Surface "assets/openscad/Advanced/surface_image.png" True

public export
surfaceImageModel : Model ThreeD
surfaceImageModel = MkModel "OpenSCAD Advanced: surface image" [] [] Satisfy $
  Union
    [ surfaceLayer 1 "indianred"
    , surfaceLayer 2 "firebrick"
    , surfaceLayer 3 "darkred"
    ]

public export
advanced : List (String, Model ThreeD)
advanced =
  [ ("advanced-geb", gebModel)
  , ("advanced-animation", animationModel)
  , ("advanced-assert", assertModel)
  , ("advanced-children", childrenModel)
  , ("advanced-children-indexed", childrenIndexedModel)
  , ("advanced-module-recursion", moduleRecursionModel)
  , ("advanced-offset", offsetModel)
  , ("advanced-surface-image", surfaceImageModel)
  ]
