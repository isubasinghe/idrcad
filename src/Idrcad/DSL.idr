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

||| A dimension can be known now or left in a finite integer domain for the
||| solver. The common constructors below use millimetres without introducing
||| floating-point literals.
public export
data DimensionSpec
  = Exactly Fixed
  | Somewhere ParameterSpec

public export
exactly : Integer -> DimensionSpec
exactly = Exactly . mm

public export
exactlyFixed : Fixed -> DimensionSpec
exactlyFixed = Exactly

midpoint : Fixed -> Fixed -> Fixed
midpoint (MkFixed lower) (MkFixed upper) =
  MkFixed ((lower + upper) `div` 2)

public export
between : Integer -> Integer -> DimensionSpec
between lower upper = Somewhere $
  midpoint (mm lower) (mm upper) `within` (mm lower `to` mm upper)

public export
betweenFixed : Fixed -> Fixed -> DimensionSpec
betweenFixed lower upper = Somewhere $
  midpoint lower upper `within` (lower `to` upper)

||| Choose the unsolved OpenSCAD preview value for a bounded dimension.
||| Solving may replace it with any value inside the original bounds.
public export
starting : Integer -> DimensionSpec -> DimensionSpec
starting value (Exactly fixed) = Exactly fixed
starting value (Somewhere (WithDefault oldDefault bounds)) =
  Somewhere (mm value `within` bounds)

public export
startingFixed : Fixed -> DimensionSpec -> DimensionSpec
startingFixed value (Exactly fixed) = Exactly fixed
startingFixed value (Somewhere (WithDefault oldDefault bounds)) =
  Somewhere (value `within` bounds)

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
  draftNextId : Nat

emptyBuild : BuildState
emptyBuild = Building [] [] Satisfy 0

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

||| Declare a parameter without inventing or maintaining a global name. The
||| hint remains in generated files for readability; the suffix is automatic.
public export
freshParameter : String -> ParameterSpec -> Design Expr
freshParameter hint (WithDefault defaultValue (BoundedBy lower upper)) =
  Designing $ \state =>
    let name = hint ++ "_" ++ show state.draftNextId
        declaration = MkParameter name lower defaultValue upper
        next = { draftParameters := declaration :: state.draftParameters,
                 draftNextId := S state.draftNextId } state
     in (Var name, next)

materializeDimension : String -> DimensionSpec -> Design Expr
materializeDimension hint (Exactly value) = pure (exact value)
materializeDimension hint (Somewhere specification) =
  freshParameter hint specification

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

registerConstraint : Constraint -> Design ()
registerConstraint constraint = Designing $ \state =>
  let next = { draftConstraints := constraint :: state.draftConstraints } state
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

||| Finish a design whose authoring layer can report an error. This is useful
||| for textual frontends: parsing and name resolution remain outside the core
||| geometry language, while successfully elaborated models use the same IR.
public export
designEither :
  String ->
  Design (Either error (Shape dimension)) ->
  Either error (Model dimension)
designEither name (Designing action) =
  let (result, state) = action emptyBuild
   in case result of
        Left problem => Left problem
        Right geometry => Right $ MkModel
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

||| Make a plate while leaving any subset of its dimensions for the solver.
||| Generated parameter names are deliberately an implementation detail.
public export
plate : DimensionSpec -> DimensionSpec -> DimensionSpec -> Design RectangularSolid
plate widthSpec depthSpec heightSpec = do
  width <- materializeDimension "width" widthSpec
  depth <- materializeDimension "depth" depthSpec
  height <- materializeDimension "height" heightSpec
  positive "Plate width" width
  positive "Plate depth" depth
  positive "Plate height" height
  pure (rectangular width depth height)

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

||| Anything with an axis-aligned plan-view footprint can participate in the
||| same containment, alignment, spacing, and packing vocabulary.
public export
interface HasFootprint feature where
  footprint : feature -> Footprint2D

public export
HasFootprint RectangularSolid where
  footprint rectangle = Footprint
    0 0 rectangle.rectangleWidth rectangle.rectangleDepth

public export
HasFootprint Hole where
  footprint hole =
    let Point x y z = hole.holeAxis
     in Footprint
          (x - hole.holeRadius)
          (y - hole.holeRadius)
          (2 * hole.holeRadius)
          (2 * hole.holeRadius)

public export
HasFootprint RectangularCutout where
  footprint cutout =
    let Point x y z = cutout.cutoutCentre
     in Footprint
          (x - cutout.cutoutHalfWidth)
          (y - cutout.cutoutHalfDepth)
          cutout.cutoutWidth
          cutout.cutoutDepth

public export
HasFootprint Footprint2D where
  footprint = id

rightEdge : Footprint2D -> Expr
rightEdge box = box.footprintX + box.footprintWidth

backEdge : Footprint2D -> Expr
backEdge box = box.footprintY + box.footprintDepth

twiceCentreX : Footprint2D -> Expr
twiceCentreX box = 2 * box.footprintX + box.footprintWidth

twiceCentreY : Footprint2D -> Expr
twiceCentreY box = 2 * box.footprintY + box.footprintDepth

||| Require the entire inner footprint to remain inside the outer footprint.
public export
inside2D :
  (HasFootprint inner, HasFootprint outer) =>
  inner -> outer -> Design ()
inside2D inner outer =
  let innerBox = footprint inner
      outerBox = footprint outer
   in do
        assert (outerBox.footprintX .<=. innerBox.footprintX)
          "Feature must remain inside the left edge"
        assert (rightEdge innerBox .<=. rightEdge outerBox)
          "Feature must remain inside the right edge"
        assert (outerBox.footprintY .<=. innerBox.footprintY)
          "Feature must remain inside the front edge"
        assert (backEdge innerBox .<=. backEdge outerBox)
          "Feature must remain inside the back edge"

||| Align plan-view centres without division, preserving the integer-linear
||| fragment even when widths are odd numbers of fixed-point ticks.
public export
alignX :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Design ()
alignX left right = assert
  (twiceCentreX (footprint left) .==. twiceCentreX (footprint right))
  "Features must be horizontally aligned"

public export
alignY :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Design ()
alignY lower upper = assert
  (twiceCentreY (footprint lower) .==. twiceCentreY (footprint upper))
  "Features must be vertically aligned"

public export
centeredIn :
  (HasFootprint inner, HasFootprint outer) =>
  inner -> outer -> Design ()
centeredIn inner outer = do
  alignX inner outer
  alignY inner outer

public export
leftOfBy :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Fixed -> Design ()
leftOfBy left right gap = assert
  (rightEdge (footprint left) + exact gap .<=. (footprint right).footprintX)
  "Features must keep their horizontal spacing"

public export
rightOfBy :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Fixed -> Design ()
rightOfBy right left gap = leftOfBy left right gap

public export
aboveBy :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Fixed -> Design ()
aboveBy upper lower gap = assert
  (backEdge (footprint lower) + exact gap .<=. (footprint upper).footprintY)
  "Features must keep their vertical spacing"

public export
belowBy :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Fixed -> Design ()
belowBy lower upper gap = aboveBy upper lower gap

||| Whole-millimetre conveniences cover the common layout case while the
||| `*By` forms retain exact micron/nanometre control.
public export
leftOf :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Integer -> Design ()
leftOf left right gap = leftOfBy left right (mm gap)

public export
rightOf :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Integer -> Design ()
rightOf right left gap = rightOfBy right left (mm gap)

public export
above :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Integer -> Design ()
above upper lower gap = aboveBy upper lower (mm gap)

public export
below :
  (HasFootprint a, HasFootprint b) =>
  a -> b -> Integer -> Design ()
below lower upper gap = belowBy lower upper (mm gap)

public export
betweenColumnsBy :
  (HasFootprint a, HasFootprint b, HasFootprint c) =>
  a -> b -> c -> Fixed -> Design ()
betweenColumnsBy middle left right gap = do
  rightOfBy middle left gap
  leftOfBy middle right gap

public export
betweenRowsBy :
  (HasFootprint a, HasFootprint b, HasFootprint c) =>
  a -> b -> c -> Fixed -> Design ()
betweenRowsBy middle lower upper gap = do
  aboveBy middle lower gap
  belowBy middle upper gap

public export
betweenColumns :
  (HasFootprint a, HasFootprint b, HasFootprint c) =>
  a -> b -> c -> Integer -> Design ()
betweenColumns middle left right gap =
  betweenColumnsBy middle left right (mm gap)

public export
betweenRows :
  (HasFootprint a, HasFootprint b, HasFootprint c) =>
  a -> b -> c -> Integer -> Design ()
betweenRows middle lower upper gap =
  betweenRowsBy middle lower upper (mm gap)

||| Let the finite-domain solver choose whether each pair separates along x
||| or y. This is lowered as MiniZinc's native `diffn` global constraint.
public export
noOverlap : List Footprint2D -> Design ()
noOverlap footprints = registerConstraint $
  NonOverlapping footprints "Features must not overlap"

spacedFootprint : Fixed -> Footprint2D -> Footprint2D
spacedFootprint gap box =
  Footprint
    box.footprintX
    box.footprintY
    (box.footprintWidth + exact gap)
    (box.footprintDepth + exact gap)

||| Require a true edge-to-edge gap without fractional arithmetic. Whichever
||| footprint precedes the other along the chosen axis carries the extra gap.
public export
spaced : Fixed -> List Footprint2D -> Design ()
spaced gap footprints = registerConstraint $
  NonOverlapping
    (map (spacedFootprint gap) footprints)
    "Features must keep their requested edge-to-edge spacing"

public export
record RectProfile where
  constructor Profile
  profileWidth : Fixed
  profileDepth : Fixed
  profileClearance : Fixed

||| A nominal whole-millimetre rectangular component profile.
public export
rect : Integer -> Integer -> RectProfile
rect width depth = Profile (mm width) (mm depth) (nanometres 0)

export infixl 5 `withClearance`

public export
withClearance : RectProfile -> Fixed -> RectProfile
withClearance (Profile width depth oldClearance) clearance =
  Profile width depth clearance

findParameter : String -> List Parameter -> Maybe Parameter
findParameter requested [] = Nothing
findParameter requested (parameter :: rest) =
  if requested == parameter.parameterName
    then Just parameter
    else findParameter requested rest

extentDefaults : Expr -> List Parameter -> Maybe (Fixed, Fixed)
extentDefaults expression declarations =
  case expression of
    Lit value => Just (value, value)
    Var name =>
      case findParameter name declarations of
        Just parameter => Just (parameter.defaultValue, parameter.upperBound)
        Nothing => Nothing
    _ => Nothing

halfFixed : Fixed -> Fixed
halfFixed (MkFixed value) = MkFixed (value `div` 2)

positionParameter : String -> Expr -> Design Expr
positionParameter hint extent = Designing $ \state =>
  let (previewExtent, maximumExtent) =
        case extentDefaults extent state.draftParameters of
          Just values => values
          Nothing => (mm 100, mm 200)
      name = hint ++ "_" ++ show state.draftNextId
      declaration = MkParameter
        name
        (nanometres 0)
        (halfFixed previewExtent)
        maximumExtent
      next = { draftParameters := declaration :: state.draftParameters,
               draftNextId := S state.draftNextId } state
   in (Var name, next)

freePosition : RectangularSolid -> Design Point3D
freePosition rectangle = do
  x <- positionParameter "x" rectangle.rectangleWidth
  y <- positionParameter "y" rectangle.rectangleDepth
  pure (point3 x y (half rectangle.rectangleHeight))

||| Place a cutout somewhere in a plate; containment is automatic and its x/y
||| coordinates remain anonymous solver variables.
public export
cutoutIn : RectangularSolid -> RectProfile -> Design RectangularCutout
cutoutIn rectangle (Profile width depth clearance) = do
  position <- freePosition rectangle
  rectangularCutout "Cutout" width depth clearance position rectangle

public export
centeredCutout : RectangularSolid -> RectProfile -> Design RectangularCutout
centeredCutout rectangle profile = do
  cutout <- cutoutIn rectangle profile
  centeredIn cutout rectangle
  pure cutout

||| Place a circular through-feature somewhere in a plate. Relationships can
||| subsequently determine both coordinates.
public export
boreIn : RectangularSolid -> Fixed -> Design Hole
boreIn rectangle radius = do
  position <- freePosition rectangle
  exact radius `at` position `through` rectangle

public export
centeredBore : RectangularSolid -> Fixed -> Design Hole
centeredBore rectangle radius = do
  hole <- boreIn rectangle radius
  centeredIn hole rectangle
  pure hole

public export
record CornerBores where
  constructor AtCorners
  lowerLeftBore : Hole
  lowerRightBore : Hole
  upperLeftBore : Hole
  upperRightBore : Hole

||| Four mounting bores are a structural pattern rather than eight decision
||| coordinates. Their axes are derived from panel size, radius, and clearance.
public export
cornerBores : RectangularSolid -> Fixed -> Fixed -> Design CornerBores
cornerBores rectangle edgeClearance radius =
  let radiusExpr = exact radius
      inset = radiusExpr + exact edgeClearance
      z = half rectangle.rectangleHeight
   in do
        lowerLeft <- radiusExpr
          `at` point3 inset inset z
          `through` rectangle
        lowerRight <- radiusExpr
          `at` point3 (rectangle.rectangleWidth - inset) inset z
          `through` rectangle
        upperLeft <- radiusExpr
          `at` point3 inset (rectangle.rectangleDepth - inset) z
          `through` rectangle
        upperRight <- radiusExpr
          `at` point3
            (rectangle.rectangleWidth - inset)
            (rectangle.rectangleDepth - inset)
            z
          `through` rectangle
        pure (AtCorners lowerLeft lowerRight upperLeft upperRight)

public export
minimumPlate : RectangularSolid -> Design ()
minimumPlate rectangle =
  minimize (rectangle.rectangleWidth + rectangle.rectangleDepth)

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
