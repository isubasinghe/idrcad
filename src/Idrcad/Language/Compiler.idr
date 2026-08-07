module Idrcad.Language.Compiler

import Data.String
import Idrcad.DSL
import Idrcad.Language.Parser
import Idrcad.Language.Syntax
import Text.FC
import Text.ParseError

%default total

public export
data SemanticIssue
  = DuplicateFeature String
  | UnknownFeature String
  | ExpectedGeometry String
  | ExpectedTwoDimensional String
  | ExpectedThreeDimensional String
  | ExpectedPlate String
  | ExpectedSingleFeature String
  | UnknownPatternMember String String
  | InvalidDimensionRange Fixed Fixed
  | MultipleRootPlates
  | MultipleSolidOutputs
  | MissingOutput

public export
Interpolation SemanticIssue where
  interpolate (DuplicateFeature name) =
    "Feature '" ++ name ++ "' has already been declared"
  interpolate (UnknownFeature name) =
    "Unknown feature '" ++ name ++ "'"
  interpolate (ExpectedGeometry name) =
    "Feature '" ++ name ++ "' is a constraint feature, not reusable geometry"
  interpolate (ExpectedTwoDimensional name) =
    "Geometry '" ++ name ++ "' is 3D where a 2D shape is required"
  interpolate (ExpectedThreeDimensional name) =
    "Geometry '" ++ name ++ "' is 2D where a 3D solid is required"
  interpolate (ExpectedPlate name) =
    "Feature '" ++ name ++ "' is not a plate and cannot contain a cut"
  interpolate (ExpectedSingleFeature name) =
    "Feature '" ++ name ++ "' is a pattern; select one member or use it in space"
  interpolate (UnknownPatternMember pattern member) =
    "Pattern '" ++ pattern ++ "' has no member named '" ++ member ++ "'"
  interpolate (InvalidDimensionRange lower upper) =
    "Invalid dimension range " ++ renderFixed lower ++ "mm.."
      ++ renderFixed upper ++ "mm: the lower bound exceeds the upper bound"
  interpolate MultipleRootPlates =
    "This language version renders one root plate per model"
  interpolate MultipleSolidOutputs =
    "A model can contain only one `solid` output statement"
  interpolate MissingOutput =
    "The model needs either a root plate or a `solid` output"

public export
data LanguageError
  = ParseFailure (ParseError SourceParseIssue)
  | SemanticFailure (FCErr SemanticIssue)

public export
Interpolation LanguageError where
  interpolate (ParseFailure problem) = interpolate problem
  interpolate (SemanticFailure problem) = interpolate problem

data FeatureValue
  = PlateValue RectangularSolid
  | CutoutValue RectangularCutout
  | BoreValue Hole
  | CornersValue CornerBores
  | Geometry2DValue (Shape TwoD)
  | Geometry3DValue (Shape ThreeD)

record FeatureEntry where
  constructor Entry
  entryName : String
  entryValue : FeatureValue

record CompilerState where
  constructor Compiling
  compilerFeatures : List FeatureEntry
  compilerRoot : Maybe RectangularSolid
  compilerCutters : List (Shape ThreeD)
  compilerSolid : Maybe (Shape ThreeD)

emptyCompiler : CompilerState
emptyCompiler = Compiling [] Nothing [] Nothing

lookupEntry : String -> List FeatureEntry -> Maybe FeatureEntry
lookupEntry requested [] = Nothing
lookupEntry requested (entry :: rest) =
  if requested == entry.entryName
    then Just entry
    else lookupEntry requested rest

alreadyDeclared : Bounded String -> CompilerState -> Bool
alreadyDeclared name state = case lookupEntry name.val state.compilerFeatures of
  Just entry => True
  Nothing => False

failureAt : Bounded value -> SemanticIssue -> Either (Bounded SemanticIssue) result
failureAt source problem = Left (B problem source.bounds)

addEntry : FeatureEntry -> CompilerState -> CompilerState
addEntry entry state =
  { compilerFeatures := entry :: state.compilerFeatures } state

addCutter : Shape ThreeD -> CompilerState -> CompilerState
addCutter cutter state =
  { compilerCutters := cutter :: state.compilerCutters } state

addCutters : List (Shape ThreeD) -> CompilerState -> CompilerState
addCutters [] state = state
addCutters (cutter :: rest) state =
  addCutters rest (addCutter cutter state)

dimensionSpec :
  Bounded String ->
  SourceDimension ->
  Either (Bounded SemanticIssue) DimensionSpec
dimensionSpec source (Known value) = Right (exactlyFixed value)
dimensionSpec source (InRange lower upper) =
  let MkFixed lowerValue = lower
      MkFixed upperValue = upper
   in if lowerValue <= upperValue
        then Right (betweenFixed lower upper)
        else failureAt source (InvalidDimensionRange lower upper)

parentPlate :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) RectangularSolid
parentPlate reference state =
  case lookupEntry reference.val state.compilerFeatures of
    Nothing => failureAt reference (UnknownFeature reference.val)
    Just (Entry name (PlateValue plate)) => Right plate
    Just entry => failureAt reference (ExpectedPlate reference.val)

splitReference : String -> (String, Maybe String)
splitReference source =
  let (base, suffix) = break (== '.') source
   in case unpack suffix of
        '.' :: member => (base, Just (pack member))
        _ => (source, Nothing)

cornerMembers : CornerBores -> List Footprint2D
cornerMembers corners =
  [ footprint corners.lowerLeftBore
  , footprint corners.lowerRightBore
  , footprint corners.upperLeftBore
  , footprint corners.upperRightBore
  ]

memberFootprint :
  String ->
  String ->
  CornerBores ->
  Maybe Footprint2D
memberFootprint pattern "lower_left" corners =
  Just (footprint corners.lowerLeftBore)
memberFootprint pattern "lower_right" corners =
  Just (footprint corners.lowerRightBore)
memberFootprint pattern "upper_left" corners =
  Just (footprint corners.upperLeftBore)
memberFootprint pattern "upper_right" corners =
  Just (footprint corners.upperRightBore)
memberFootprint pattern member corners = Nothing

entryFootprints : FeatureEntry -> List Footprint2D
entryFootprints (Entry name (PlateValue plate)) = [footprint plate]
entryFootprints (Entry name (CutoutValue cutout)) = [footprint cutout]
entryFootprints (Entry name (BoreValue bore)) = [footprint bore]
entryFootprints (Entry name (CornersValue corners)) = cornerMembers corners
entryFootprints (Entry name (Geometry2DValue shape)) = []
entryFootprints (Entry name (Geometry3DValue shape)) = []

resolveMany :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (List Footprint2D)
resolveMany reference state =
  case lookupEntry reference.val state.compilerFeatures of
    Just entry => Right (entryFootprints entry)
    Nothing =>
      let (base, member) = splitReference reference.val
       in case (lookupEntry base state.compilerFeatures, member) of
            (Nothing, _) => failureAt reference (UnknownFeature reference.val)
            (Just (Entry name (CornersValue corners)), Just selected) =>
              case memberFootprint base selected corners of
                Just value => Right [value]
                Nothing => failureAt reference
                  (UnknownPatternMember base selected)
            (Just entry, Just selected) => failureAt reference
              (UnknownPatternMember base selected)
            (Just entry, Nothing) => Right (entryFootprints entry)

resolveOne :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) Footprint2D
resolveOne reference state = case resolveMany reference state of
  Left problem => Left problem
  Right [value] => Right value
  Right values => failureAt reference (ExpectedSingleFeature reference.val)

resolveTwo :
  Bounded String ->
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (Footprint2D, Footprint2D)
resolveTwo first second state = case resolveOne first state of
  Left problem => Left problem
  Right left => case resolveOne second state of
    Left problem => Left problem
    Right right => Right (left, right)

resolveThree :
  Bounded String ->
  Bounded String ->
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (Footprint2D, Footprint2D, Footprint2D)
resolveThree first second third state = case resolveOne first state of
  Left problem => Left problem
  Right one => case resolveOne second state of
    Left problem => Left problem
    Right two => case resolveOne third state of
      Left problem => Left problem
      Right three => Right (one, two, three)

resolveAll :
  List (Bounded String) ->
  CompilerState ->
  Either (Bounded SemanticIssue) (List Footprint2D)
resolveAll [] state = Right []
resolveAll (reference :: rest) state = case resolveMany reference state of
  Left problem => Left problem
  Right values => case resolveAll rest state of
    Left problem => Left problem
    Right remaining => Right (values ++ remaining)

data AnyGeometry
  = Any2D (Shape TwoD)
  | Any3D (Shape ThreeD)

resolveGeometry :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) AnyGeometry
resolveGeometry reference state =
  case lookupEntry reference.val state.compilerFeatures of
    Nothing => failureAt reference (UnknownFeature reference.val)
    Just (Entry name (Geometry2DValue shape)) => Right (Any2D shape)
    Just (Entry name (Geometry3DValue shape)) => Right (Any3D shape)
    Just entry => failureAt reference (ExpectedGeometry reference.val)

resolve2D :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (Shape TwoD)
resolve2D reference state = case resolveGeometry reference state of
  Left problem => Left problem
  Right (Any2D shape) => Right shape
  Right (Any3D shape) =>
    failureAt reference (ExpectedTwoDimensional reference.val)

resolve3D :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (Shape ThreeD)
resolve3D reference state = case resolveGeometry reference state of
  Left problem => Left problem
  Right (Any3D shape) => Right shape
  Right (Any2D shape) =>
    failureAt reference (ExpectedThreeDimensional reference.val)

resolveGeometryList :
  List (Bounded String) ->
  CompilerState ->
  Either (Bounded SemanticIssue) (List AnyGeometry)
resolveGeometryList [] state = Right []
resolveGeometryList (reference :: rest) state =
  case resolveGeometry reference state of
    Left problem => Left problem
    Right value => case resolveGeometryList rest state of
      Left problem => Left problem
      Right values => Right (value :: values)

all2D :
  List (Bounded String) ->
  CompilerState ->
  Either (Bounded SemanticIssue) (List (Shape TwoD))
all2D [] state = Right []
all2D (reference :: rest) state = case resolve2D reference state of
  Left problem => Left problem
  Right shape => case all2D rest state of
    Left problem => Left problem
    Right shapes => Right (shape :: shapes)

all3D :
  List (Bounded String) ->
  CompilerState ->
  Either (Bounded SemanticIssue) (List (Shape ThreeD))
all3D [] state = Right []
all3D (reference :: rest) state = case resolve3D reference state of
  Left problem => Left problem
  Right shape => case all3D rest state of
    Left problem => Left problem
    Right shapes => Right (shape :: shapes)

compileStatements :
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)

continueWith :
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)

addGeometry :
  Bounded String ->
  FeatureValue ->
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)
addGeometry name value rest state =
  if alreadyDeclared name state
    then pure (failureAt name (DuplicateFeature name.val))
    else continueWith rest (addEntry (Entry name.val value) state)

lit : Fixed -> Expr
lit = Lit

toVec2 : SourcePoint2 -> Vec2
toVec2 (Point2 x y) = MkVec2 (lit x) (lit y)

toVec3 : SourcePoint3 -> Vec3
toVec3 (Point3 x y z) = MkVec3 (lit x) (lit y) (lit z)

indices : Nat -> List Nat
indices count = go 0 count
  where
    go : Nat -> Nat -> List Nat
    go index Z = []
    go index (S remaining) = index :: go (S index) remaining

natExpr : Nat -> Expr
natExpr value = integer (cast value)

angleAt : Nat -> Nat -> Expr
angleAt index count =
  divide (multiply (integer 360) (natExpr index)) (natExpr count)

regularPoint : Nat -> Fixed -> Nat -> Vec2
regularPoint count radius index =
  let angle = angleAt index count
   in MkVec2 (lit radius * cosine angle) (lit radius * sine angle)

regularPolygon : Nat -> Fixed -> Shape TwoD
regularPolygon count radius = Polygon $
  map (regularPoint count radius) (indices count)

isEven : Nat -> Bool
isEven Z = True
isEven (S Z) = False
isEven (S (S rest)) = isEven rest

starPoint : Nat -> Fixed -> Fixed -> Nat -> Vec2
starPoint count inner outer index =
  let radius = if isEven index then inner else outer
      angle = angleAt index count
   in MkVec2 (lit radius * cosine angle) (lit radius * sine angle)

starShape : Nat -> Fixed -> Fixed -> Shape TwoD
starShape points inner outer =
  let count = points + points
   in Polygon $ map (starPoint count inner outer) (indices count)

ringShape : Nat -> Fixed -> Shape ThreeD -> Shape ThreeD
ringShape count radius child = Union $ map place (indices count)
  where
    place : Nat -> Shape ThreeD
    place index = Rotate3D (MkVec3 0 0 (angleAt index count)) $
      Translate3D (MkVec3 (lit radius) 0 0) child

roundedBoxShape : Fixed -> Fixed -> Fixed -> Fixed -> Shape ThreeD
roundedBoxShape width depth height radius =
  let x = divide (lit width) (integer 2) - lit radius
      y = divide (lit depth) (integer 2) - lit radius
      z = divide (lit height) (integer 2) - lit radius
      corners =
        [ MkVec3 (-x) (-y) (-z), MkVec3 (-x) (-y) z
        , MkVec3 (-x) y (-z), MkVec3 (-x) y z
        , MkVec3 x (-y) (-z), MkVec3 x (-y) z
        , MkVec3 x y (-z), MkVec3 x y z
        ]
   in Hull $ map (\point => Translate3D point (Sphere (lit radius))) corners

treeShape : Nat -> Expr -> Expr -> Shape TwoD
treeShape Z length thickness = Square (MkVec2 thickness length) False
treeShape (S level) length thickness =
  let trunk = Square (MkVec2 thickness length) False
      nextLength = lit (MkFixed 700000) * length
      nextThickness = lit (MkFixed 800000) * thickness
      left = Translate2D (MkVec2 0 length) $
        Rotate2D 28 (treeShape level nextLength nextThickness)
      right = Translate2D (MkVec2 0 length) $
        Rotate2D (-31) (treeShape level nextLength nextThickness)
   in Union [trunk, left, right]

continueWith statements state = compileStatements statements state

compileStatement :
  SourceStatement ->
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)
compileStatement (GeometryBox name width depth height center) rest state =
  addGeometry name
    (Geometry3DValue (Cube (MkVec3 (lit width) (lit depth) (lit height)) center))
    rest state
compileStatement (GeometryRoundedBox name width depth height radius) rest state =
  addGeometry name
    (Geometry3DValue (roundedBoxShape width depth height radius)) rest state
compileStatement (GeometrySphere name radius) rest state =
  addGeometry name (Geometry3DValue (Sphere (lit radius))) rest state
compileStatement (GeometryCylinder name height bottom top center) rest state =
  addGeometry name
    (Geometry3DValue (Cylinder (lit height) (lit bottom) (lit top) center))
    rest state
compileStatement (GeometrySquare name width depth center) rest state =
  addGeometry name
    (Geometry2DValue (Square (MkVec2 (lit width) (lit depth)) center))
    rest state
compileStatement (GeometryCircle name radius) rest state =
  addGeometry name (Geometry2DValue (Circle (lit radius))) rest state
compileStatement (GeometryPolygon name points) rest state =
  addGeometry name (Geometry2DValue (Polygon (map toVec2 points))) rest state
compileStatement (GeometryRegularPolygon name sides radius) rest state =
  addGeometry name (Geometry2DValue (regularPolygon sides radius)) rest state
compileStatement (GeometryStar name points inner outer) rest state =
  addGeometry name (Geometry2DValue (starShape points inner outer)) rest state
compileStatement (GeometryText name value size) rest state =
  addGeometry name
    (Geometry2DValue (Text2D
      (MkTextOptions value (lit size) Nothing AlignCenter AlignCenterV 1)))
    rest state
compileStatement (GeometryImport2D name path) rest state =
  addGeometry name (Geometry2DValue (Import2D path Nothing)) rest state
compileStatement (GeometryImport3D name path) rest state =
  addGeometry name (Geometry3DValue (Import3D path)) rest state
compileStatement (GeometrySurface name path center) rest state =
  addGeometry name (Geometry3DValue (Surface path center)) rest state
compileStatement (GeometryPolyhedron name points faces) rest state =
  addGeometry name
    (Geometry3DValue (Polyhedron (map toVec3 points) faces)) rest state
compileStatement (GeometryUnion name references) rest state =
  case references of
    [] => addGeometry name (Geometry3DValue (Union [])) rest state
    first :: remaining => case resolveGeometry first state of
      Left problem => pure (Left problem)
      Right (Any2D shape) => case all2D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry2DValue (Union (shape :: shapes))) rest state
      Right (Any3D shape) => case all3D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry3DValue (Union (shape :: shapes))) rest state
compileStatement (GeometryDifference name base cutters) rest state =
  case resolveGeometry base state of
    Left problem => pure (Left problem)
    Right (Any2D shape) => case all2D cutters state of
      Left problem => pure (Left problem)
      Right shapes => addGeometry name
        (Geometry2DValue (Difference shape shapes)) rest state
    Right (Any3D shape) => case all3D cutters state of
      Left problem => pure (Left problem)
      Right shapes => addGeometry name
        (Geometry3DValue (Difference shape shapes)) rest state
compileStatement (GeometryIntersection name references) rest state =
  case references of
    [] => addGeometry name (Geometry3DValue (Intersection [])) rest state
    first :: remaining => case resolveGeometry first state of
      Left problem => pure (Left problem)
      Right (Any2D shape) => case all2D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry2DValue (Intersection (shape :: shapes))) rest state
      Right (Any3D shape) => case all3D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry3DValue (Intersection (shape :: shapes))) rest state
compileStatement (GeometryHull name references) rest state =
  case references of
    [] => addGeometry name (Geometry3DValue (Hull [])) rest state
    first :: remaining => case resolveGeometry first state of
      Left problem => pure (Left problem)
      Right (Any2D shape) => case all2D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry2DValue (Hull (shape :: shapes))) rest state
      Right (Any3D shape) => case all3D remaining state of
        Left problem => pure (Left problem)
        Right shapes => addGeometry name
          (Geometry3DValue (Hull (shape :: shapes))) rest state
compileStatement (GeometryMove2 name child x y) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry2DValue (Translate2D (MkVec2 (lit x) (lit y)) shape)) rest state
compileStatement (GeometryMove3 name child x y z) rest state =
  case resolve3D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (Translate3D (MkVec3 (lit x) (lit y) (lit z)) shape))
      rest state
compileStatement (GeometryRotate2 name child rotation) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry2DValue (Rotate2D (lit rotation) shape)) rest state
compileStatement (GeometryRotate3 name child x y z) rest state =
  case resolve3D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (Rotate3D (MkVec3 (lit x) (lit y) (lit z)) shape))
      rest state
compileStatement (GeometryScale2 name child x y) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry2DValue (Scale2D (MkVec2 (lit x) (lit y)) shape)) rest state
compileStatement (GeometryScale3 name child x y z) rest state =
  case resolve3D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (Scale3D (MkVec3 (lit x) (lit y) (lit z)) shape))
      rest state
compileStatement (GeometryColour name child colour) rest state =
  case resolveGeometry child state of
    Left problem => pure (Left problem)
    Right (Any2D shape) => addGeometry name
      (Geometry2DValue (Colourise (NamedColour colour) shape)) rest state
    Right (Any3D shape) => addGeometry name
      (Geometry3DValue (Colourise (NamedColour colour) shape)) rest state
compileStatement (GeometryHighlight name child) rest state =
  case resolveGeometry child state of
    Left problem => pure (Left problem)
    Right (Any2D shape) => addGeometry name
      (Geometry2DValue (Highlight shape)) rest state
    Right (Any3D shape) => addGeometry name
      (Geometry3DValue (Highlight shape)) rest state
compileStatement (GeometryBackground name child) rest state =
  case resolveGeometry child state of
    Left problem => pure (Left problem)
    Right (Any2D shape) => addGeometry name
      (Geometry2DValue (Background shape)) rest state
    Right (Any3D shape) => addGeometry name
      (Geometry3DValue (Background shape)) rest state
compileStatement (GeometryFacets name child count) rest state =
  let resolution = MkResolution (Just count) Nothing Nothing
   in case resolveGeometry child state of
    Left problem => pure (Left problem)
    Right (Any2D shape) => addGeometry name
      (Geometry2DValue (WithResolution resolution shape)) rest state
    Right (Any3D shape) => addGeometry name
      (Geometry3DValue (WithResolution resolution shape)) rest state
compileStatement (GeometryExtrude name child height center) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (LinearExtrude
        (MkLinearExtrudeOptions (lit height) center Nothing Nothing Nothing)
        shape)) rest state
compileStatement
    (GeometryTwistExtrude name child height center twist scaling slices)
    rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (LinearExtrude
        (MkLinearExtrudeOptions
          (lit height) center (Just (lit twist))
          (Just (UniformExtrude (lit scaling))) (Just slices))
        shape)) rest state
compileStatement (GeometryRevolve name child rotation convexity) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (RotateExtrude
        (MkRotateExtrudeOptions (Just (lit rotation)) (Just convexity)) shape))
      rest state
compileStatement (GeometryProjection name child cut) rest state =
  case resolve3D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry2DValue (Projection cut shape)) rest state
compileStatement (GeometryOffset name child delta chamfer) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry2DValue (Offset2D (DeltaOffset (lit delta) chamfer) shape))
      rest state
compileStatement (GeometryRoof name child voronoi) rest state =
  case resolve2D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (Roof (if voronoi then Voronoi else StraightSkeleton) shape))
      rest state
compileStatement (GeometryRing name child count radius) rest state =
  case resolve3D child state of
    Left problem => pure (Left problem)
    Right shape => addGeometry name
      (Geometry3DValue (ringShape count radius shape)) rest state
compileStatement (GeometryTree name levels length thickness) rest state =
  addGeometry name
    (Geometry2DValue (treeShape levels (lit length) (lit thickness))) rest state
compileStatement (RequireAtLeast actual minimum message) rest state = do
  assert (lit actual .>=. lit minimum) message
  continueWith rest state
compileStatement (EmitSolid reference) rest state =
  case state.compilerSolid of
    Just shape => pure (failureAt reference MultipleSolidOutputs)
    Nothing => case resolve3D reference state of
      Left problem => pure (Left problem)
      Right shape => continueWith rest ({ compilerSolid := Just shape } state)
compileStatement (DeclarePlate name (MkPlateFields width depth height)) rest state =
  if alreadyDeclared name state
    then pure (failureAt name (DuplicateFeature name.val))
    else case state.compilerRoot of
      Just root => pure (failureAt name MultipleRootPlates)
      Nothing => case (dimensionSpec name width,
                       dimensionSpec name depth,
                       dimensionSpec name height) of
        (Right widthSpec, Right depthSpec, Right heightSpec) => do
          value <- plate widthSpec depthSpec heightSpec
          let next = addEntry (Entry name.val (PlateValue value)) $
                { compilerRoot := Just value } state
          continueWith rest next
        (Left problem, _, _) => pure (Left problem)
        (_, Left problem, _) => pure (Left problem)
        (_, _, Left problem) => pure (Left problem)
compileStatement
    (DeclareCutout name (MkCutoutFields width depth clearance) parent)
    rest state =
  if alreadyDeclared name state
    then pure (failureAt name (DuplicateFeature name.val))
    else case parentPlate parent state of
      Left problem => pure (Left problem)
      Right plate => do
        value <- cutoutIn plate $
          Profile width depth clearance
        let next = addCutter value.cutoutShape $
              addEntry (Entry name.val (CutoutValue value)) state
        continueWith rest next
compileStatement (DeclareBore name (MkBoreFields radius) parent) rest state =
  if alreadyDeclared name state
    then pure (failureAt name (DuplicateFeature name.val))
    else case parentPlate parent state of
      Left problem => pure (Left problem)
      Right plate => do
        value <- boreIn plate radius
        let next = addCutter value.holeShape $
              addEntry (Entry name.val (BoreValue value)) state
        continueWith rest next
compileStatement
    (DeclareCornerBores name (MkCornerBoreFields radius edge) parent)
    rest state =
  if alreadyDeclared name state
    then pure (failureAt name (DuplicateFeature name.val))
    else case parentPlate parent state of
      Left problem => pure (Left problem)
      Right plate => do
        value <- cornerBores plate edge radius
        let cutters =
              [ value.lowerLeftBore.holeShape
              , value.lowerRightBore.holeShape
              , value.upperLeftBore.holeShape
              , value.upperRightBore.holeShape
              ]
            next = addCutters cutters $
              addEntry (Entry name.val (CornersValue value)) state
        continueWith rest next
compileStatement (Center inner outer) rest state =
  case resolveTwo inner outer state of
    Left problem => pure (Left problem)
    Right (inside, outside) => do
      centeredIn inside outside
      continueWith rest state
compileStatement (AlignX first second) rest state =
  case resolveTwo first second state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      alignX one two
      continueWith rest state
compileStatement (AlignY first second) rest state =
  case resolveTwo first second state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      alignY one two
      continueWith rest state
compileStatement (LeftOf left right gap) rest state =
  case resolveTwo left right state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      leftOfBy one two gap
      continueWith rest state
compileStatement (RightOf right left gap) rest state =
  case resolveTwo right left state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      rightOfBy one two gap
      continueWith rest state
compileStatement (Above upper lower gap) rest state =
  case resolveTwo upper lower state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      aboveBy one two gap
      continueWith rest state
compileStatement (Below lower upper gap) rest state =
  case resolveTwo lower upper state of
    Left problem => pure (Left problem)
    Right (one, two) => do
      belowBy one two gap
      continueWith rest state
compileStatement (BetweenColumns middle left right gap) rest state =
  case resolveThree middle left right state of
    Left problem => pure (Left problem)
    Right (one, two, three) => do
      betweenColumnsBy one two three gap
      continueWith rest state
compileStatement (BetweenRows middle lower upper gap) rest state =
  case resolveThree middle lower upper state of
    Left problem => pure (Left problem)
    Right (one, two, three) => do
      betweenRowsBy one two three gap
      continueWith rest state
compileStatement (Space references gap) rest state =
  case resolveAll references state of
    Left problem => pure (Left problem)
    Right values => do
      spaced gap values
      continueWith rest state
compileStatement (Minimize reference) rest state =
  case parentPlate reference state of
    Left problem => pure (Left problem)
    Right plate => do
      minimumPlate plate
      continueWith rest state

compileStatements [] state = pure (Right state)
compileStatements (statement :: rest) state =
  compileStatement statement rest state

finalGeometry :
  Bounded String ->
  CompilerState ->
  Either (Bounded SemanticIssue) (Shape ThreeD)
finalGeometry modelName state = case state.compilerRoot of
  Nothing => case state.compilerSolid of
    Nothing => failureAt modelName MissingOutput
    Just shape => Right shape
  Just root => case state.compilerSolid of
    Just shape => Right shape
    Nothing => Right $ facets 64 $ colour "steelblue" $
      cutFeatures root (reverse state.compilerCutters)

buildProgram :
  SourceProgram ->
  Design (Either (Bounded SemanticIssue) (Shape ThreeD))
buildProgram program = do
  result <- compileStatements program.sourceStatements emptyCompiler
  pure $ case result of
    Left problem => Left problem
    Right state => finalGeometry program.sourceModelName state

elaborate :
  SourceProgram ->
  Either (Bounded SemanticIssue) (Model ThreeD)
elaborate program =
  designEither program.sourceModelName.val (buildProgram program)

public export
compileSource :
  Origin -> String -> Either LanguageError (Model ThreeD)
compileSource origin source = case parseSource origin source of
  Left problem => Left (ParseFailure problem)
  Right program => case elaborate program of
    Left problem => Left $
      SemanticFailure (toParseError origin source problem)
    Right model => Right model
