module Idrcad.Examples.Util

import public Idrcad.DSL

%default total

public export
e : Integer -> Expr
e = integer

public export
f : Integer -> Expr
f value = exact (millionths value)

public export
v2 : Integer -> Integer -> Vec2
v2 x y = MkVec2 (e x) (e y)

public export
v2f : Integer -> Integer -> Vec2
v2f x y = MkVec2 (f x) (f y)

public export
v3 : Integer -> Integer -> Integer -> Vec3
v3 x y z = MkVec3 (e x) (e y) (e z)

public export
v3f : Integer -> Integer -> Integer -> Vec3
v3f x y z = MkVec3 (f x) (f y) (f z)

public export
named : String -> Shape dimension -> Shape dimension
named name = Colourise (NamedColour name)

public export
cubeOf : Integer -> Bool -> Shape ThreeD
cubeOf size = Cube (uniform3 (e size))

public export
sphereOf : Integer -> Shape ThreeD
sphereOf radius = Sphere (e radius)

public export
cylinderOf : Integer -> Integer -> Bool -> Shape ThreeD
cylinderOf height radius = Cylinder (e height) (e radius) (e radius)

public export
resolution : Maybe Nat -> Maybe Fixed -> Maybe Fixed -> Resolution
resolution fragments angle size =
  MkResolution fragments (map exact angle) (map exact size)

natInteger : Nat -> Integer
natInteger Z = 0
natInteger (S value) = 1 + natInteger value

public export
natExpr : Nat -> Expr
natExpr = e . natInteger

rangeFrom : Nat -> Nat -> List Nat
rangeFrom start Z = []
rangeFrom start (S count) = start :: rangeFrom (S start) count

public export
indices : Nat -> List Nat
indices = rangeFrom 0

enumerateFrom : Nat -> List value -> List (Nat, value)
enumerateFrom index [] = []
enumerateFrom index (value :: rest) =
  (index, value) :: enumerateFrom (S index) rest

public export
enumerate : List value -> List (Nat, value)
enumerate = enumerateFrom 0

public export
angleAt : (index : Nat) -> (count : Nat) -> Expr
angleAt index count = (360 * natExpr index) `dividedBy` natExpr count

||| Idris functions replace OpenSCAD modules taking `children()`: the child is
||| an ordinary, dimension-indexed value.
public export
ring3D : (radius : Expr) -> (count : Nat) -> Shape ThreeD -> Shape ThreeD
ring3D radius count child = Union $ map place (indices count)
  where
    place : Nat -> Shape ThreeD
    place index = Rotate3D (MkVec3 0 0 (angleAt index count)) $
      Translate3D (MkVec3 radius 0 0) child

public export
ring2D : (radius : Expr) -> (count : Nat) -> Shape TwoD -> Shape TwoD
ring2D radius count child = Union $ map place (indices count)
  where
    place : Nat -> Shape TwoD
    place index = Rotate2D (angleAt index count) $
      Translate2D (MkVec2 radius 0) child

public export
thin : Shape TwoD -> Shape ThreeD
thin = LinearExtrude (MkLinearExtrudeOptions 1 False Nothing Nothing Nothing)

public export
extrude : Expr -> Shape TwoD -> Shape ThreeD
extrude height = LinearExtrude
  (MkLinearExtrudeOptions height False Nothing Nothing Nothing)

public export
centeredExtrude : Expr -> Shape TwoD -> Shape ThreeD
centeredExtrude height = LinearExtrude
  (MkLinearExtrudeOptions height True Nothing Nothing Nothing)

public export
text2 : String -> Expr -> Shape TwoD
text2 content size = Text2D $
  MkTextOptions content size Nothing AlignCenter AlignCenterV 1

public export
text3 : String -> Expr -> Expr -> Shape ThreeD
text3 content size height = extrude height (text2 content size)

public export
regularPolygon : Nat -> Expr -> Shape TwoD
regularPolygon sides radius =
  WithResolution (resolution (Just sides) Nothing Nothing) (Circle radius)

public export
empty3D : Shape ThreeD
empty3D = Union []
