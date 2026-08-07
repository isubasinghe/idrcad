module Idrcad.Language.Syntax

import public Idrcad.Fixed
import public Text.Bounds

%default total

||| A source dimension is either known or deliberately left in a finite range.
||| Both forms have already been converted to exact fixed-point millimetres.
public export
data SourceDimension
  = Known Fixed
  | InRange Fixed Fixed

public export
record PlateFields where
  constructor MkPlateFields
  sourceWidth : SourceDimension
  sourceDepth : SourceDimension
  sourceHeight : SourceDimension

public export
record CutoutFields where
  constructor MkCutoutFields
  sourceCutoutWidth : Fixed
  sourceCutoutDepth : Fixed
  sourceCutoutClearance : Fixed

public export
record BoreFields where
  constructor MkBoreFields
  sourceBoreRadius : Fixed

public export
record CornerBoreFields where
  constructor MkCornerBoreFields
  sourceCornerRadius : Fixed
  sourceCornerEdge : Fixed

public export
record SourcePoint2 where
  constructor Point2
  point2X : Fixed
  point2Y : Fixed

public export
record SourcePoint3 where
  constructor Point3
  point3X : Fixed
  point3Y : Fixed
  point3Z : Fixed

||| The deliberately small first language surface. Every identifier remains
||| bounded by its source location so semantic errors can point at one token.
public export
data SourceStatement
  = DeclarePlate (Bounded String) PlateFields
  | DeclareCutout (Bounded String) CutoutFields (Bounded String)
  | DeclareBore (Bounded String) BoreFields (Bounded String)
  | DeclareCornerBores (Bounded String) CornerBoreFields (Bounded String)
  | Center (Bounded String) (Bounded String)
  | AlignX (Bounded String) (Bounded String)
  | AlignY (Bounded String) (Bounded String)
  | LeftOf (Bounded String) (Bounded String) Fixed
  | RightOf (Bounded String) (Bounded String) Fixed
  | Above (Bounded String) (Bounded String) Fixed
  | Below (Bounded String) (Bounded String) Fixed
  | BetweenColumns
      (Bounded String) (Bounded String) (Bounded String) Fixed
  | BetweenRows
      (Bounded String) (Bounded String) (Bounded String) Fixed
  | Space (List (Bounded String)) Fixed
  | Minimize (Bounded String)
  | RequireAtLeast Fixed Fixed String
  | GeometryBox (Bounded String) Fixed Fixed Fixed Bool
  | GeometryRoundedBox (Bounded String) Fixed Fixed Fixed Fixed
  | GeometrySphere (Bounded String) Fixed
  | GeometryCylinder (Bounded String) Fixed Fixed Fixed Bool
  | GeometrySquare (Bounded String) Fixed Fixed Bool
  | GeometryCircle (Bounded String) Fixed
  | GeometryPolygon (Bounded String) (List SourcePoint2)
  | GeometryRegularPolygon (Bounded String) Nat Fixed
  | GeometryStar (Bounded String) Nat Fixed Fixed
  | GeometryText (Bounded String) String Fixed
  | GeometryImport2D (Bounded String) String
  | GeometryImport3D (Bounded String) String
  | GeometrySurface (Bounded String) String Bool
  | GeometryPolyhedron
      (Bounded String) (List SourcePoint3) (List (List Nat))
  | GeometryUnion (Bounded String) (List (Bounded String))
  | GeometryDifference
      (Bounded String) (Bounded String) (List (Bounded String))
  | GeometryIntersection (Bounded String) (List (Bounded String))
  | GeometryHull (Bounded String) (List (Bounded String))
  | GeometryMove2 (Bounded String) (Bounded String) Fixed Fixed
  | GeometryMove3 (Bounded String) (Bounded String) Fixed Fixed Fixed
  | GeometryRotate2 (Bounded String) (Bounded String) Fixed
  | GeometryRotate3 (Bounded String) (Bounded String) Fixed Fixed Fixed
  | GeometryScale2 (Bounded String) (Bounded String) Fixed Fixed
  | GeometryScale3 (Bounded String) (Bounded String) Fixed Fixed Fixed
  | GeometryColour (Bounded String) (Bounded String) String
  | GeometryHighlight (Bounded String) (Bounded String)
  | GeometryBackground (Bounded String) (Bounded String)
  | GeometryFacets (Bounded String) (Bounded String) Nat
  | GeometryExtrude (Bounded String) (Bounded String) Fixed Bool
  | GeometryTwistExtrude
      (Bounded String) (Bounded String) Fixed Bool Fixed Fixed Nat
  | GeometryRevolve (Bounded String) (Bounded String) Fixed Nat
  | GeometryProjection (Bounded String) (Bounded String) Bool
  | GeometryOffset (Bounded String) (Bounded String) Fixed Bool
  | GeometryRoof (Bounded String) (Bounded String) Bool
  | GeometryRing (Bounded String) (Bounded String) Nat Fixed
  | GeometryTree (Bounded String) Nat Fixed Fixed
  | EmitSolid (Bounded String)

public export
record SourceProgram where
  constructor MkSourceProgram
  sourceModelName : Bounded String
  sourceStatements : List SourceStatement
