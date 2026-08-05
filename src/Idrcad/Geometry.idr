module Idrcad.Geometry

import Idrcad.Expr

%default total

public export
data Dimension = TwoD | ThreeD

public export
record Vec2 where
  constructor MkVec2
  x2 : Expr
  y2 : Expr

public export
record Vec3 where
  constructor MkVec3
  x3 : Expr
  y3 : Expr
  z3 : Expr

public export
vec2 : Expr -> Expr -> Vec2
vec2 = MkVec2

public export
vec3 : Expr -> Expr -> Expr -> Vec3
vec3 = MkVec3

public export
uniform2 : Expr -> Vec2
uniform2 value = MkVec2 value value

public export
uniform3 : Expr -> Vec3
uniform3 value = MkVec3 value value value

public export
data HorizontalAlignment = AlignLeft | AlignCenter | AlignRight

public export
data VerticalAlignment = AlignTop | AlignCenterV | AlignBaseline | AlignBottom

public export
record TextOptions where
  constructor MkTextOptions
  textContent : String
  textSize : Expr
  textFont : Maybe String
  textHorizontal : HorizontalAlignment
  textVertical : VerticalAlignment
  textSpacing : Expr

public export
defaultText : String -> Expr -> TextOptions
defaultText content size =
  MkTextOptions content size Nothing AlignLeft AlignBaseline (integer 1)

public export
data ExtrudeScale = UniformExtrude Expr | XYExtrude Vec2

public export
record LinearExtrudeOptions where
  constructor MkLinearExtrudeOptions
  linearHeight : Expr
  linearCenter : Bool
  linearTwist : Maybe Expr
  linearScale : Maybe ExtrudeScale
  linearConvexity : Maybe Nat

public export
defaultLinearExtrude : Expr -> LinearExtrudeOptions
defaultLinearExtrude height =
  MkLinearExtrudeOptions height False Nothing Nothing Nothing

public export
record RotateExtrudeOptions where
  constructor MkRotateExtrudeOptions
  rotateAngle : Maybe Expr
  rotateConvexity : Maybe Nat

public export
defaultRotateExtrude : RotateExtrudeOptions
defaultRotateExtrude = MkRotateExtrudeOptions Nothing Nothing

public export
data RoofMethod = StraightSkeleton | Voronoi

public export
data Colour
  = NamedColour String
  | RGB Expr Expr Expr
  | RGBA Expr Expr Expr Expr

public export
record Resolution where
  constructor MkResolution
  fragments : Maybe Nat
  minimumAngle : Maybe Expr
  minimumSize : Maybe Expr

public export
noResolution : Resolution
noResolution = MkResolution Nothing Nothing Nothing

||| A dimension-indexed OpenSCAD geometry tree. Operations that change
||| dimension make that transition explicit in their result type.
public export
data Shape : Dimension -> Type where
  Square : Vec2 -> Bool -> Shape TwoD
  Circle : Expr -> Shape TwoD
  Polygon : List Vec2 -> Shape TwoD
  Text2D : TextOptions -> Shape TwoD

  Cube : Vec3 -> Bool -> Shape ThreeD
  Sphere : Expr -> Shape ThreeD
  Cylinder : Expr -> Expr -> Expr -> Bool -> Shape ThreeD
  Import3D : String -> Shape ThreeD

  Union : List (Shape dimension) -> Shape dimension
  Difference : Shape dimension -> List (Shape dimension) -> Shape dimension
  Intersection : List (Shape dimension) -> Shape dimension
  Hull : List (Shape dimension) -> Shape dimension

  Translate2D : Vec2 -> Shape TwoD -> Shape TwoD
  Translate3D : Vec3 -> Shape ThreeD -> Shape ThreeD
  Rotate2D : Expr -> Shape TwoD -> Shape TwoD
  Rotate3D : Vec3 -> Shape ThreeD -> Shape ThreeD
  Scale2D : Vec2 -> Shape TwoD -> Shape TwoD
  Scale3D : Vec3 -> Shape ThreeD -> Shape ThreeD

  Colourise : Colour -> Shape dimension -> Shape dimension
  Highlight : Shape dimension -> Shape dimension
  Background : Shape dimension -> Shape dimension
  WithResolution : Resolution -> Shape dimension -> Shape dimension

  LinearExtrude : LinearExtrudeOptions -> Shape TwoD -> Shape ThreeD
  RotateExtrude : RotateExtrudeOptions -> Shape TwoD -> Shape ThreeD
  Projection : Bool -> Shape ThreeD -> Shape TwoD
  Roof : RoofMethod -> Shape TwoD -> Shape ThreeD

public export
rectangle : Expr -> Expr -> Bool -> Shape TwoD
rectangle width height = Square (MkVec2 width height)

public export
box : Expr -> Expr -> Expr -> Bool -> Shape ThreeD
box width depth height = Cube (MkVec3 width depth height)

public export
cylinder : Expr -> Expr -> Bool -> Shape ThreeD
cylinder radius height = Cylinder height radius radius
