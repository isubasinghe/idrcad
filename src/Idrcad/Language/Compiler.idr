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
  | ExpectedPlate String
  | ExpectedSingleFeature String
  | UnknownPatternMember String String
  | InvalidDimensionRange Fixed Fixed
  | MultipleRootPlates
  | MissingRootPlate

public export
Interpolation SemanticIssue where
  interpolate (DuplicateFeature name) =
    "Feature '" ++ name ++ "' has already been declared"
  interpolate (UnknownFeature name) =
    "Unknown feature '" ++ name ++ "'"
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
  interpolate MissingRootPlate =
    "The model does not declare a root plate"

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

record FeatureEntry where
  constructor Entry
  entryName : String
  entryValue : FeatureValue

record CompilerState where
  constructor Compiling
  compilerFeatures : List FeatureEntry
  compilerRoot : Maybe RectangularSolid
  compilerCutters : List (Shape ThreeD)

emptyCompiler : CompilerState
emptyCompiler = Compiling [] Nothing []

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

compileStatements :
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)

continueWith :
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)
continueWith statements state = compileStatements statements state

compileStatement :
  SourceStatement ->
  List SourceStatement ->
  CompilerState ->
  Design (Either (Bounded SemanticIssue) CompilerState)
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
  Nothing => failureAt modelName MissingRootPlate
  Just root => Right $ facets 64 $ colour "steelblue" $
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
