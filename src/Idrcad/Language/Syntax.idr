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

public export
record SourceProgram where
  constructor MkSourceProgram
  sourceModelName : Bounded String
  sourceStatements : List SourceStatement
