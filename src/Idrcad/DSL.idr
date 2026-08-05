module Idrcad.DSL

import public Idrcad.Constraint
import public Idrcad.Expr
import public Idrcad.Fixed
import public Idrcad.Geometry
import public Idrcad.Model

%default total

||| Symbolic arithmetic. Numeric literals are exact whole CAD units; the
||| resulting tree is still checked by the selected backend before lowering.
public export
Num Expr where
  (+) = Add
  (*) = Multiply
  fromInteger = integer

public export
Neg Expr where
  (-) = Subtract
  negate = Negate

||| One millimetre is the conventional whole unit used by the supplied CAD
||| examples. These constructors never pass through a floating-point value.
public export
mm : Integer -> Fixed
mm = whole

public export
microns : Integer -> Fixed
microns = thousandths

public export
nanometres : Integer -> Fixed
nanometres = millionths

||| Embed an exact physical value into a symbolic expression.
public export
exact : Fixed -> Expr
exact = Lit

||| Explicit symbolic division. It is useful for geometry, but the integer
||| solver fragment deliberately rejects it when it occurs in a constraint.
public export
dividedBy : Expr -> Expr -> Expr
dividedBy = Divide

public export
half : Expr -> Expr
half value@(Lit fixed) =
  case divideExact fixed (whole 2) of
    Just result => Lit result
    Nothing => value `dividedBy` 2
half value = value `dividedBy` 2

public export
record Bounds where
  constructor BoundedBy
  lower : Fixed
  upper : Fixed

export infix 5 `to`

public export
to : Fixed -> Fixed -> Bounds
to = BoundedBy

public export
record ParameterSpec where
  constructor WithDefault
  defaultValue : Fixed
  bounds : Bounds

export infix 4 `within`

||| Describe a parameter's default and finite solver domain.
public export
within : Fixed -> Bounds -> ParameterSpec
within = WithDefault

||| A relation waiting to be given a diagnostic message by `assert`.
public export
record Predicate where
  constructor Relates
  left : Expr
  relation : Relation
  right : Expr

export infix 4 .==., .<., .<=., .>., .>=.

public export
(.==.) : Expr -> Expr -> Predicate
left .==. right = Relates left Equal right

public export
(.<.) : Expr -> Expr -> Predicate
left .<. right = Relates left LessThan right

public export
(.<=.) : Expr -> Expr -> Predicate
left .<=. right = Relates left LessOrEqual right

public export
(.>.) : Expr -> Expr -> Predicate
left .>. right = Relates left GreaterThan right

public export
(.>=.) : Expr -> Expr -> Predicate
left .>=. right = Relates left GreaterOrEqual right

record BuildState where
  constructor Building
  draftParameters : List Parameter
  draftConstraints : List Constraint
  draftObjective : Objective

emptyBuild : BuildState
emptyBuild = Building [] [] Satisfy

||| A pure model-building computation. It only collects declarations and
||| constraints; no IO or solver invocation occurs while authoring a model.
public export
record Design value where
  constructor Designing
  runDesign : BuildState -> (value, BuildState)

public export
Functor Design where
  map transform (Designing action) = Designing $ \state =>
    let (value, next) = action state
     in (transform value, next)

public export
Applicative Design where
  pure value = Designing (\state => (value, state))
  (Designing function) <*> (Designing argument) = Designing $ \state =>
    let (transform, afterFunction) = function state
        (value, afterArgument) = argument afterFunction
     in (transform value, afterArgument)

public export
Monad Design where
  (Designing action) >>= continue = Designing $ \state =>
    let (value, next) = action state
        Designing following = continue value
     in following next

||| Declare a bounded decision parameter and receive its symbolic value.
public export
parameter : String -> ParameterSpec -> Design Expr
parameter name (WithDefault defaultValue (BoundedBy lower upper)) =
  Designing $ \state =>
    let declaration = MkParameter name lower defaultValue upper
        next = { draftParameters := declaration :: state.draftParameters } state
     in (Var name, next)

||| A common non-negative manufacturing tolerance parameter.
public export
tolerance : String -> (defaultValue : Fixed) -> (maximum : Fixed) -> Design Expr
tolerance name defaultValue maximum =
  parameter name (defaultValue `within` (nanometres 0 `to` maximum))

||| Add an assertion to the design. The message is retained by both the
||| OpenSCAD and MiniZinc backends.
public export
assert : Predicate -> String -> Design ()
assert (Relates left relation right) message = Designing $ \state =>
  let constraint = Constrain left relation right message
      next = { draftConstraints := constraint :: state.draftConstraints } state
   in ((), next)

public export
positive : String -> Expr -> Design ()
positive label value = assert (value .>. 0) (label ++ " must be positive")

public export
nonNegative : String -> Expr -> Design ()
nonNegative label value = assert (value .>=. 0) (label ++ " cannot be negative")

||| Ask the solver for the greatest feasible value of a linear expression.
public export
maximize : Expr -> Design ()
maximize expression = Designing $ \state =>
  ((), { draftObjective := Maximize expression } state)

||| Ask the solver for the least feasible value of a linear expression.
public export
minimize : Expr -> Design ()
minimize expression = Designing $ \state =>
  ((), { draftObjective := Minimize expression } state)

||| Finish a pure design block as a dimension-indexed CAD model.
public export
design : String -> Design (Shape dimension) -> Model dimension
design name (Designing action) =
  let (geometry, state) = action emptyBuild
   in MkModel
        name
        (reverse state.draftParameters)
        (reverse state.draftConstraints)
        state.draftObjective
        geometry

||| Mark the geometry returned by a design block.
public export
solid : Shape dimension -> Design (Shape dimension)
solid = pure

||| Natural geometry vocabulary used by small designs.
public export
centeredBox : Expr -> Expr -> Expr -> Shape ThreeD
centeredBox width depth height = Cube (MkVec3 width depth height) True

public export
centeredCylinder : Expr -> Expr -> Shape ThreeD
centeredCylinder radius height = Cylinder height radius radius True

public export
union : List (Shape dimension) -> Shape dimension
union = Union

public export
cut : Shape dimension -> List (Shape dimension) -> Shape dimension
cut = Difference

public export
move3 : Expr -> Expr -> Expr -> Shape ThreeD -> Shape ThreeD
move3 x y z = Translate3D (MkVec3 x y z)

public export
move2 : Expr -> Expr -> Shape TwoD -> Shape TwoD
move2 x y = Translate2D (MkVec2 x y)

public export
colour : String -> Shape dimension -> Shape dimension
colour name = Colourise (NamedColour name)

public export
facets : Nat -> Shape dimension -> Shape dimension
facets count = WithResolution (MkResolution (Just count) Nothing Nothing)

||| A symbolic point. Sharing a Point3D between features makes coincidence a
||| construction-time fact instead of another equation the solver can violate.
public export
record Point3D where
  constructor Point
  pointX : Expr
  pointY : Expr
  pointZ : Expr

public export
point3 : Expr -> Expr -> Expr -> Point3D
point3 = Point

public export
record RectangularSolid where
  constructor Rectangle
  rectangleWidth : Expr
  rectangleDepth : Expr
  rectangleHeight : Expr
  rectangleShape : Shape ThreeD

||| An axis-aligned x-by-y-by-z solid whose local origin is its lower corner.
public export
rectangular : Expr -> Expr -> Expr -> RectangularSolid
rectangular width depth height =
  Rectangle width depth height (Cube (MkVec3 width depth height) False)

||| The center is derived symbolically: (x / 2, y / 2, z / 2).
public export
centreOf : RectangularSolid -> Point3D
centreOf rectangle = Point
  (half rectangle.rectangleWidth)
  (half rectangle.rectangleDepth)
  (half rectangle.rectangleHeight)

public export
record HolePlacement where
  constructor PlaceHole
  placementRadius : Expr
  placementPoint : Point3D

export infixl 5 `at`

public export
at : Expr -> Point3D -> HolePlacement
at = PlaceHole

public export
record Hole where
  constructor MkHole
  holeRadius : Expr
  holeAxis : Point3D
  holeShape : Shape ThreeD

export infixl 5 `through`

||| Cut a circular feature through a rectangular solid. Its depth comes from
||| the rectangle; only radius and position need to be described.
public export
through : HolePlacement -> RectangularSolid -> Design Hole
through (PlaceHole radius point) rectangle =
  let Point x y z = point
      cutter = Translate3D (MkVec3 x y z) $
        Cylinder (rectangle.rectangleHeight + 2) radius radius True
   in do
        positive "Hole radius" radius
        assert (radius .<=. x) "Hole must remain inside the rectangle's left edge"
        assert (x + radius .<=. rectangle.rectangleWidth)
          "Hole must remain inside the rectangle's right edge"
        assert (radius .<=. y) "Hole must remain inside the rectangle's front edge"
        assert (y + radius .<=. rectangle.rectangleDepth)
          "Hole must remain inside the rectangle's back edge"
        pure (MkHole radius point cutter)

||| Material remaining after applying a hole feature.
public export
drilled : RectangularSolid -> Hole -> Shape ThreeD
drilled rectangle hole = Difference rectangle.rectangleShape [hole.holeShape]

||| A rectangular through-cut together with the dimensions and centre used to
||| constrain neighbouring features. Width and depth include the requested
||| per-side clearance.
public export
record RectangularCutout where
  constructor MkRectangularCutout
  cutoutWidth : Expr
  cutoutDepth : Expr
  cutoutHalfWidth : Expr
  cutoutHalfDepth : Expr
  cutoutCentre : Point3D
  cutoutShape : Shape ThreeD

||| Cut a nominal rectangular component profile through a rectangular solid.
||| `clearance` is added on both sides of both dimensions, using exact fixed-
||| point arithmetic. The feature also registers four edge-containment
||| constraints against its parent solid.
public export
rectangularCutout :
  (name : String) ->
  (nominalWidth : Fixed) ->
  (nominalDepth : Fixed) ->
  (clearance : Fixed) ->
  Point3D ->
  RectangularSolid ->
  Design RectangularCutout
rectangularCutout name nominalWidth nominalDepth clearance point rectangle =
  let width = exact (addFixed nominalWidth (addFixed clearance clearance))
      depth = exact (addFixed nominalDepth (addFixed clearance clearance))
      halfWidth = half width
      halfDepth = half depth
      Point x y z = point
      cutter = Translate3D (MkVec3 x y z) $
        Cube (MkVec3 width depth (rectangle.rectangleHeight + 2)) True
   in do
        positive (name ++ " width") width
        positive (name ++ " depth") depth
        assert (halfWidth .<=. x)
          (name ++ " must remain inside the rectangle's left edge")
        assert (x + halfWidth .<=. rectangle.rectangleWidth)
          (name ++ " must remain inside the rectangle's right edge")
        assert (halfDepth .<=. y)
          (name ++ " must remain inside the rectangle's front edge")
        assert (y + halfDepth .<=. rectangle.rectangleDepth)
          (name ++ " must remain inside the rectangle's back edge")
        pure (MkRectangularCutout
          width depth halfWidth halfDepth point cutter)

||| Remove several already-constrained features from a rectangular solid.
public export
cutFeatures :
  RectangularSolid ->
  List (Shape ThreeD) ->
  Shape ThreeD
cutFeatures rectangle features = Difference rectangle.rectangleShape features

public export
record FitAllowance where
  constructor Allowance
  minimumClearance : Fixed
  partTolerance : Fixed
  holeTolerance : Fixed

||| Worst-case radial allowances: clearance, mating-part tolerance, then hole
||| tolerance. All three are exact fixed-point quantities.
public export
allowing : Fixed -> Fixed -> Fixed -> FitAllowance
allowing = Allowance

public export
record CylinderFeature where
  constructor FittedCylinder
  cylinderRadius : Expr
  cylinderAxis : Point3D
  cylinderShape : Shape ThreeD

||| Create a solver-sized cylinder whose axis is definitionally the same point
||| as the hole axis. Only its radius is free; placement is inherited.
public export
fittedCylinder :
  (name : String) ->
  Hole ->
  ParameterSpec ->
  (height : Expr) ->
  FitAllowance ->
  Design CylinderFeature
fittedCylinder name hole radiusSpec height allowance = do
  radius <- parameter (name ++ "_radius") radiusSpec
  let maximumRadius = radius
        + exact allowance.partTolerance
        + exact allowance.minimumClearance
        + exact allowance.holeTolerance
  assert (maximumRadius .<=. hole.holeRadius)
    (name ++ " must fit inside its hole at worst-case tolerances")
  positive (name ++ " radius") radius
  let Point x y z = hole.holeAxis
      shape = Translate3D (MkVec3 x y z) $
        Cylinder height radius radius True
  pure (FittedCylinder radius hole.holeAxis shape)
