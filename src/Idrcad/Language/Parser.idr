module Idrcad.Language.Parser

import Data.List1
import Data.String
import Idrcad.Language.Syntax
import Text.Parse.Manual
import Text.Parse.Syntax

%default total

%hide Prelude.(<*>)
%hide Prelude.(*>)
%hide Prelude.(<*)
%hide Prelude.pure

public export
data SourceParseIssue
  = InvalidMeasurement String
  | InvalidNatural String

public export
Eq SourceParseIssue where
  InvalidMeasurement left == InvalidMeasurement right = left == right
  InvalidNatural left == InvalidNatural right = left == right
  _ == _ = False

public export
Interpolation SourceParseIssue where
  interpolate (InvalidMeasurement value) =
    "Invalid exact millimetre measurement: " ++ value
      ++ ". Use at most six decimal places."
  interpolate (InvalidNatural value) =
    "Invalid non-negative whole number: " ++ value

data Token
  = Word String
  | Number String
  | StringValue String
  | Symbol Char
  | DotDot
  | Ignored

Eq Token where
  Word left == Word right = left == right
  Number left == Number right = left == right
  StringValue left == StringValue right = left == right
  Symbol left == Symbol right = left == right
  DotDot == DotDot = True
  Ignored == Ignored = True
  _ == _ = False

Interpolation Token where
  interpolate (Word value) = value
  interpolate (Number value) = value
  interpolate (StringValue value) = "\"" ++ value ++ "\""
  interpolate (Symbol value) = singleton value
  interpolate DotDot = ".."
  interpolate Ignored = "comment"

isNameChar : Char -> Bool
isNameChar character =
  isAlphaNum character
    || character == '_'
    || character == '-'

word : SnocList Char -> AutoTok lexError Token
word characters ('.' :: '.' :: rest) =
  Succ (Word (cast characters)) ('.' :: '.' :: rest)
word characters ('.' :: next :: rest) =
  if isAlpha next || next == '_'
    then word (characters :< '.' :< next) rest
    else Succ (Word (cast characters)) ('.' :: next :: rest)
word characters (next :: rest) =
  if isNameChar next
    then word (characters :< next) rest
    else Succ (Word (cast characters)) (next :: rest)
word characters [] = Succ (Word (cast characters)) []

fraction : SnocList Char -> AutoTok lexError Token
fraction characters (next :: rest) =
  if isDigit next
    then fraction (characters :< next) rest
    else Succ (Number (cast characters)) (next :: rest)
fraction characters [] = Succ (Number (cast characters)) []

number : SnocList Char -> AutoTok lexError Token
number characters ('.' :: '.' :: rest) =
  Succ (Number (cast characters)) ('.' :: '.' :: rest)
number characters ('.' :: next :: rest) =
  if isDigit next
    then fraction (characters :< '.' :< next) rest
    else Succ (Number (cast characters)) ('.' :: next :: rest)
number characters (next :: rest) =
  if isDigit next
    then number (characters :< next) rest
    else Succ (Number (cast characters)) (next :: rest)
number characters [] = Succ (Number (cast characters)) []

comment : AutoTok lexError Token
comment ('\n' :: rest) = Succ Ignored ('\n' :: rest)
comment (next :: rest) = comment rest
comment [] = Succ Ignored []

stringToken : SnocList Char -> AutoTok lexError Token
stringToken characters ('"' :: rest) =
  Succ (StringValue (cast characters)) rest
stringToken characters (next :: rest) =
  stringToken (characters :< next) rest
stringToken characters [] = eoiAt p

term : Tok True lexError Token
term ('=' :: rest) = Succ (Symbol '=') rest
term ('(' :: rest) = Succ (Symbol '(') rest
term (')' :: rest) = Succ (Symbol ')') rest
term (',' :: rest) = Succ (Symbol ',') rest
term ('[' :: rest) = Succ (Symbol '[') rest
term (']' :: rest) = Succ (Symbol ']') rest
term ('{' :: rest) = Succ (Symbol '{') rest
term ('}' :: rest) = Succ (Symbol '}') rest
term (';' :: rest) = Succ (Symbol ';') rest
term ('-' :: rest) = Succ (Symbol '-') rest
term ('"' :: rest) = stringToken [<] rest
term ('.' :: '.' :: rest) = Succ DotDot rest
term ('#' :: rest) = comment rest
term (next :: rest) =
  if isDigit next
    then number [<next] rest
    else if isAlpha next || next == '_'
      then word [<next] rest
      else unknown Same
term [] = eoiAt Same

lexTokens :
  SnocList (Bounded Token) ->
  Position ->
  (characters : List Char) ->
  (0 accessible : SuffixAcc characters) ->
  Either (Bounded (InnerError Void)) (List (Bounded Token))
lexTokens tokens position (next :: rest) (SA recurse) =
  if isSpace next
    then lexTokens tokens (Text.Bounds.next next position) rest recurse
    else case term (next :: rest) of
      Succ token remaining @{consumed} =>
        let end = endPos position consumed
            boundedToken = bounded token position end
            nextTokens = case token of
              Ignored => tokens
              _ => tokens :< boundedToken
         in lexTokens nextTokens end remaining recurse
      Fail start errorEnd problem =>
        Left (boundedErr position start errorEnd problem)
lexTokens tokens position [] accessible = Right (tokens <>> [])

lexSource : String -> Either (Bounded (InnerError Void)) (List (Bounded Token))
lexSource source = lexTokens [<] begin (unpack source) suffixAcc

padding : Nat -> Integer -> Integer
padding 0 value = value * 1000000
padding 1 value = value * 100000
padding 2 value = value * 10000
padding 3 value = value * 1000
padding 4 value = value * 100
padding 5 value = value * 10
padding _ value = value

fractionValue : List Char -> Integer
fractionValue = foldl
  (\value, character => value * 10 + cast (digit character))
  0

fixedFromString : String -> Maybe Fixed
fixedFromString source =
  let (wholePart, fractionalPart) = break (== '.') source
   in case parseInteger {a = Integer} wholePart of
        Nothing => Nothing
        Just wholeValue => case unpack fractionalPart of
          [] => Just (MkFixed (wholeValue * fixedScale))
          '.' :: digits =>
            if length digits <= 6 && all isDigit digits && not (null digits)
              then Just $ MkFixed
                (wholeValue * fixedScale
                  + padding (length digits) (fractionValue digits))
              else Nothing
          _ => Nothing

0 Rule : Bool -> Type -> Type
Rule strict value = Grammar strict Token SourceParseIssue value

identifier : Rule True (Bounded String)
identifier (B (Word value) bounds :: rest) = Succ0 (B value bounds) rest
identifier tokens = terminal (const Nothing) tokens

punctuation : Char -> Rule True ()
punctuation = exact . Symbol

keyword : String -> Rule True ()
keyword = exact . Word

measurement : Rule True Fixed
measurement = terminalE convert <* keyword "mm"
  where
    convert : Token -> Either SourceParseIssue Fixed
    convert (Number value) = case fixedFromString value of
      Just fixed => Right fixed
      Nothing => Left (InvalidMeasurement value)
    convert token = Left (InvalidMeasurement (interpolate token))

scalar : Rule True Fixed
scalar = terminalE convert
  where
    convert : Token -> Either SourceParseIssue Fixed
    convert (Number value) = case fixedFromString value of
      Just fixed => Right fixed
      Nothing => Left (InvalidMeasurement value)
    convert token = Left (InvalidMeasurement (interpolate token))

signed : Rule True Fixed -> Rule True Fixed
signed value tokens@(B (Symbol '-') _ :: rest) =
  [| negateFixed (punctuation '-' *> value) |] tokens
signed value tokens = value tokens

signedMeasurement : Rule True Fixed
signedMeasurement = signed measurement

signedScalar : Rule True Fixed
signedScalar = signed scalar

angle : Rule True Fixed
angle = signedScalar <* keyword "deg"

natural : Rule True Nat
natural = terminalE convert
  where
    convert : Token -> Either SourceParseIssue Nat
    convert (Number value) = case parseInteger {a = Integer} value of
      Just parsed =>
        if parsed >= 0 then Right (cast parsed) else Left (InvalidNatural value)
      Nothing => Left (InvalidNatural value)
    convert token = Left (InvalidNatural (interpolate token))

boolean : Rule True Bool
boolean (B (Word "true") bounds :: rest) = Succ0 True rest
boolean (B (Word "false") bounds :: rest) = Succ0 False rest
boolean tokens = terminal (const Nothing) tokens

stringValue : Rule True String
stringValue (B (StringValue value) bounds :: rest) = Succ0 value rest
stringValue tokens = terminal (const Nothing) tokens

vector2 : Rule True (Fixed, Fixed)
vector2 = between (Symbol '[') (Symbol ']') $
  [| MkPair (signedMeasurement <* punctuation ',') signedMeasurement |]

vector3 : Rule True (Fixed, Fixed, Fixed)
vector3 = between (Symbol '[') (Symbol ']') $
  [| MkTriple
      (signedMeasurement <* punctuation ',')
      (signedMeasurement <* punctuation ',')
      signedMeasurement
   |]
  where
    MkTriple : Fixed -> Fixed -> Fixed -> (Fixed, Fixed, Fixed)
    MkTriple x y z = (x, y, z)

scale2 : Rule True (Fixed, Fixed)
scale2 = between (Symbol '[') (Symbol ']') $
  [| MkPair (signedScalar <* punctuation ',') signedScalar |]

scale3 : Rule True (Fixed, Fixed, Fixed)
scale3 = between (Symbol '[') (Symbol ']') $
  [| MkTriple
      (signedScalar <* punctuation ',')
      (signedScalar <* punctuation ',')
      signedScalar
   |]
  where
    MkTriple : Fixed -> Fixed -> Fixed -> (Fixed, Fixed, Fixed)
    MkTriple x y z = (x, y, z)

angles3 : Rule True (Fixed, Fixed, Fixed)
angles3 = between (Symbol '[') (Symbol ']') $
  [| MkTriple
      (angle <* punctuation ',')
      (angle <* punctuation ',')
      angle
   |]
  where
    MkTriple : Fixed -> Fixed -> Fixed -> (Fixed, Fixed, Fixed)
    MkTriple x y z = (x, y, z)

fixedDimension : Rule True SourceDimension
fixedDimension = [| Known measurement |]

rangedDimension : Rule True SourceDimension
rangedDimension = [| InRange measurement (exact DotDot *> measurement) |]

sourceDimension : Rule True SourceDimension
sourceDimension tokens =
  rangedDimension tokens <|> fixedDimension tokens

field : String -> Rule True value -> Rule True value
field name value = keyword name *> punctuation '=' *> value

plateFields : Rule True PlateFields
plateFields = between (Symbol '(') (Symbol ')') $
  [| MkPlateFields
      (field "width" sourceDimension <* punctuation ',')
      (field "depth" sourceDimension <* punctuation ',')
      (field "height" sourceDimension)
   |]

cutoutFields : Rule True CutoutFields
cutoutFields = between (Symbol '(') (Symbol ')') $
  [| MkCutoutFields
      (field "width" measurement <* punctuation ',')
      (field "depth" measurement <* punctuation ',')
      (field "clearance" measurement)
   |]

boreFields : Rule True BoreFields
boreFields = between (Symbol '(') (Symbol ')') $
  [| MkBoreFields (field "radius" measurement) |]

cornerFields : Rule True CornerBoreFields
cornerFields = between (Symbol '(') (Symbol ')') $
  [| MkCornerBoreFields
      (field "radius" measurement <* punctuation ',')
      (field "edge" measurement)
   |]

atName : Bounded String -> SourceStatement -> Bounded SourceStatement
atName name statement = B statement name.bounds

makePlate : Bounded String -> PlateFields -> Bounded SourceStatement
makePlate name fields = atName name (DeclarePlate name fields)

plateStatement : Rule True (Bounded SourceStatement)
plateStatement = [| makePlate
  identifier
  (punctuation '=' *> keyword "plate" *> plateFields)
  |]

makeCutout :
  Bounded String -> CutoutFields -> Bounded String -> Bounded SourceStatement
makeCutout name fields parent =
  atName name (DeclareCutout name fields parent)

cutoutStatement : Rule True (Bounded SourceStatement)
cutoutStatement = [| makeCutout
  identifier
  (punctuation '=' *> keyword "cutout" *> cutoutFields)
  (keyword "in" *> identifier)
  |]

makeBore :
  Bounded String -> BoreFields -> Bounded String -> Bounded SourceStatement
makeBore name fields parent = atName name (DeclareBore name fields parent)

boreStatement : Rule True (Bounded SourceStatement)
boreStatement = [| makeBore
  identifier
  (punctuation '=' *> keyword "bore" *> boreFields)
  (keyword "in" *> identifier)
  |]

makeCornerBores :
  Bounded String ->
  CornerBoreFields ->
  Bounded String ->
  Bounded SourceStatement
makeCornerBores name fields parent =
  atName name (DeclareCornerBores name fields parent)

cornerStatement : Rule True (Bounded SourceStatement)
cornerStatement = [| makeCornerBores
  identifier
  (punctuation '=' *> keyword "corner_bores" *> cornerFields)
  (keyword "in" *> identifier)
  |]

makeBinary :
  (Bounded String -> Bounded String -> SourceStatement) ->
  Bounded String ->
  Bounded String ->
  Bounded SourceStatement
makeBinary constructor left right = atName left (constructor left right)

makeCenter :
  Bounded String -> Bounded String -> Bounded SourceStatement
makeCenter = makeBinary Center

makeAlignX :
  Bounded String -> Bounded String -> Bounded SourceStatement
makeAlignX = makeBinary AlignX

makeAlignY :
  Bounded String -> Bounded String -> Bounded SourceStatement
makeAlignY = makeBinary AlignY

centerStatement : Rule True (Bounded SourceStatement)
centerStatement = [| makeCenter
  (keyword "center" *> identifier)
  (keyword "in" *> identifier)
  |]

alignXStatement : Rule True (Bounded SourceStatement)
alignXStatement = [| makeAlignX
  (keyword "align_x" *> identifier)
  (keyword "with" *> identifier)
  |]

alignYStatement : Rule True (Bounded SourceStatement)
alignYStatement = [| makeAlignY
  (keyword "align_y" *> identifier)
  (keyword "with" *> identifier)
  |]

makeDirectional :
  (Bounded String -> Bounded String -> Fixed -> SourceStatement) ->
  Bounded String ->
  Bounded String ->
  Fixed ->
  Bounded SourceStatement
makeDirectional constructor subject reference gap =
  atName subject (constructor subject reference gap)

directionalStatement :
  String ->
  (Bounded String -> Bounded String -> Fixed -> SourceStatement) ->
  Rule True (Bounded SourceStatement)
directionalStatement relation constructor =
  [| buildDirectional
      identifier
      (keyword relation *> identifier)
      (keyword "by" *> measurement)
   |]
  where
    buildDirectional :
      Bounded String ->
      Bounded String ->
      Fixed ->
      Bounded SourceStatement
    buildDirectional = makeDirectional constructor

makeTernary :
  (Bounded String -> Bounded String -> Bounded String -> Fixed -> SourceStatement) ->
  Bounded String ->
  Bounded String ->
  Bounded String ->
  Fixed ->
  Bounded SourceStatement
makeTernary constructor middle first second gap =
  atName middle (constructor middle first second gap)

makeBetweenColumns :
  Bounded String ->
  Bounded String ->
  Bounded String ->
  Fixed ->
  Bounded SourceStatement
makeBetweenColumns = makeTernary BetweenColumns

makeBetweenRows :
  Bounded String ->
  Bounded String ->
  Bounded String ->
  Fixed ->
  Bounded SourceStatement
makeBetweenRows = makeTernary BetweenRows

betweenColumnsStatement : Rule True (Bounded SourceStatement)
betweenColumnsStatement = [| makeBetweenColumns
  (keyword "between_columns" *> identifier)
  identifier
  identifier
  (keyword "by" *> measurement)
  |]

betweenRowsStatement : Rule True (Bounded SourceStatement)
betweenRowsStatement = [| makeBetweenRows
  (keyword "between_rows" *> identifier)
  identifier
  identifier
  (keyword "by" *> measurement)
  |]

listFrom : List1 value -> List value
listFrom (head ::: tail) = head :: tail

listErrorIsFatal : InnerError SourceParseIssue -> Bool
listErrorIsFatal (Expected [","] value) = False
listErrorIsFatal problem = True

referenceList : Rule True (List (Bounded String))
referenceList = between (Symbol '[') (Symbol ']') $
  [| listFrom
      (sepByF1 listErrorIsFatal (punctuation ',') identifier)
   |]

makeSpace : List (Bounded String) -> Fixed -> Bounded SourceStatement
makeSpace references gap = case references of
  [] => B (Space [] gap) NoBounds
  first :: rest => atName first (Space references gap)

spaceStatement : Rule True (Bounded SourceStatement)
spaceStatement = [| makeSpace
  (keyword "space" *> referenceList)
  (keyword "by" *> measurement)
  |]

makeMinimize : Bounded String -> Bounded SourceStatement
makeMinimize reference = atName reference (Minimize reference)

minimizeStatement : Rule True (Bounded SourceStatement)
minimizeStatement = [| makeMinimize (keyword "minimize" *> identifier) |]

geometryBoxStatement : Rule True (Bounded SourceStatement)
geometryBoxStatement = [| build
  identifier
  (punctuation '=' *> keyword "box" *> between (Symbol '(') (Symbol ')')
    [| MkFields
      (field "width" measurement <* punctuation ',')
      (field "depth" measurement <* punctuation ',')
      (field "height" measurement <* punctuation ',')
      (field "center" boolean)
    |])
  |]
  where
    MkFields : Fixed -> Fixed -> Fixed -> Bool -> (Fixed, Fixed, Fixed, Bool)
    MkFields width depth height center = (width, depth, height, center)
    build : Bounded String -> (Fixed, Fixed, Fixed, Bool) -> Bounded SourceStatement
    build name (width, depth, height, center) =
      atName name (GeometryBox name width depth height center)

geometryRoundedBoxStatement : Rule True (Bounded SourceStatement)
geometryRoundedBoxStatement = [| build
  identifier
  (punctuation '=' *> keyword "rounded_box" *>
    between (Symbol '(') (Symbol ')') [| MkFields
      (field "width" measurement <* punctuation ',')
      (field "depth" measurement <* punctuation ',')
      (field "height" measurement <* punctuation ',')
      (field "radius" measurement)
    |])
  |]
  where
    MkFields : Fixed -> Fixed -> Fixed -> Fixed -> (Fixed, Fixed, Fixed, Fixed)
    MkFields width depth height radius = (width, depth, height, radius)
    build : Bounded String -> (Fixed, Fixed, Fixed, Fixed) -> Bounded SourceStatement
    build name (width, depth, height, radius) =
      atName name (GeometryRoundedBox name width depth height radius)

geometrySphereStatement : Rule True (Bounded SourceStatement)
geometrySphereStatement = [| build
  identifier
  (punctuation '=' *> keyword "sphere" *>
    between (Symbol '(') (Symbol ')') (field "radius" measurement))
  |]
  where
    build : Bounded String -> Fixed -> Bounded SourceStatement
    build name radius = atName name (GeometrySphere name radius)

geometryCylinderStatement : Rule True (Bounded SourceStatement)
geometryCylinderStatement = [| build
  identifier
  (punctuation '=' *> keyword "cylinder" *>
    between (Symbol '(') (Symbol ')') [| MkFields
      (field "height" measurement <* punctuation ',')
      (field "bottom" measurement <* punctuation ',')
      (field "top" measurement <* punctuation ',')
      (field "center" boolean)
    |])
  |]
  where
    MkFields : Fixed -> Fixed -> Fixed -> Bool -> (Fixed, Fixed, Fixed, Bool)
    MkFields height bottom top center = (height, bottom, top, center)
    build : Bounded String -> (Fixed, Fixed, Fixed, Bool) -> Bounded SourceStatement
    build name (height, bottom, top, center) =
      atName name (GeometryCylinder name height bottom top center)

geometrySquareStatement : Rule True (Bounded SourceStatement)
geometrySquareStatement = [| build
  identifier
  (punctuation '=' *> keyword "rectangle" *>
    between (Symbol '(') (Symbol ')') [| MkFields
      (field "width" measurement <* punctuation ',')
      (field "depth" measurement <* punctuation ',')
      (field "center" boolean)
    |])
  |]
  where
    MkFields : Fixed -> Fixed -> Bool -> (Fixed, Fixed, Bool)
    MkFields width depth center = (width, depth, center)
    build : Bounded String -> (Fixed, Fixed, Bool) -> Bounded SourceStatement
    build name (width, depth, center) =
      atName name (GeometrySquare name width depth center)

geometryCircleStatement : Rule True (Bounded SourceStatement)
geometryCircleStatement = [| build
  identifier
  (punctuation '=' *> keyword "circle" *>
    between (Symbol '(') (Symbol ')') (field "radius" measurement))
  |]
  where
    build : Bounded String -> Fixed -> Bounded SourceStatement
    build name radius = atName name (GeometryCircle name radius)

point2 : Rule True SourcePoint2
point2 = between (Symbol '[') (Symbol ']') $
  [| Point2 (signedMeasurement <* punctuation ',') signedMeasurement |]

point3 : Rule True SourcePoint3
point3 = between (Symbol '[') (Symbol ']') $
  [| Point3
      (signedMeasurement <* punctuation ',')
      (signedMeasurement <* punctuation ',')
      signedMeasurement
   |]

pointList2 : Rule True (List SourcePoint2)
pointList2 = between (Symbol '[') (Symbol ']') $
  [| listFrom (sepByF1 listErrorIsFatal (punctuation ',') point2) |]

pointList3 : Rule True (List SourcePoint3)
pointList3 = between (Symbol '[') (Symbol ']') $
  [| listFrom (sepByF1 listErrorIsFatal (punctuation ',') point3) |]

face : Rule True (List Nat)
face = between (Symbol '[') (Symbol ']') $
  [| listFrom (sepByF1 listErrorIsFatal (punctuation ',') natural) |]

faceList : Rule True (List (List Nat))
faceList = between (Symbol '[') (Symbol ']') $
  [| listFrom (sepByF1 listErrorIsFatal (punctuation ',') face) |]

geometryPolygonStatement : Rule True (Bounded SourceStatement)
geometryPolygonStatement = [| build
  identifier
  (punctuation '=' *> keyword "polygon" *>
    between (Symbol '(') (Symbol ')') (field "points" pointList2))
  |]
  where
    build : Bounded String -> List SourcePoint2 -> Bounded SourceStatement
    build name points = atName name (GeometryPolygon name points)

geometryRegularPolygonStatement : Rule True (Bounded SourceStatement)
geometryRegularPolygonStatement = [| build
  identifier
  (punctuation '=' *> keyword "regular_polygon" *>
    between (Symbol '(') (Symbol ')') [| MkPair
      (field "sides" natural <* punctuation ',')
      (field "radius" measurement)
    |])
  |]
  where
    build : Bounded String -> (Nat, Fixed) -> Bounded SourceStatement
    build name (sides, radius) =
      atName name (GeometryRegularPolygon name sides radius)

geometryStarStatement : Rule True (Bounded SourceStatement)
geometryStarStatement = [| build
  identifier
  (punctuation '=' *> keyword "star" *>
    between (Symbol '(') (Symbol ')') [| MkFields
      (field "points" natural <* punctuation ',')
      (field "inner" measurement <* punctuation ',')
      (field "outer" measurement)
    |])
  |]
  where
    MkFields : Nat -> Fixed -> Fixed -> (Nat, Fixed, Fixed)
    MkFields points inner outer = (points, inner, outer)
    build : Bounded String -> (Nat, Fixed, Fixed) -> Bounded SourceStatement
    build name (points, inner, outer) =
      atName name (GeometryStar name points inner outer)

geometryTextStatement : Rule True (Bounded SourceStatement)
geometryTextStatement = [| build
  identifier
  (punctuation '=' *> keyword "text" *>
    between (Symbol '(') (Symbol ')') [| MkPair
      (field "value" stringValue <* punctuation ',')
      (field "size" measurement)
    |])
  |]
  where
    build : Bounded String -> (String, Fixed) -> Bounded SourceStatement
    build name (value, size) = atName name (GeometryText name value size)

geometryImportStatement : String -> Bool -> Rule True (Bounded SourceStatement)
geometryImportStatement kind is3D = [| build
  identifier
  (punctuation '=' *> keyword kind *>
    between (Symbol '(') (Symbol ')') (field "path" stringValue))
  |]
  where
    build : Bounded String -> String -> Bounded SourceStatement
    build name path = atName name $
      if is3D then GeometryImport3D name path else GeometryImport2D name path

geometrySurfaceStatement : Rule True (Bounded SourceStatement)
geometrySurfaceStatement = [| build
  identifier
  (punctuation '=' *> keyword "surface" *>
    between (Symbol '(') (Symbol ')') [| MkPair
      (field "path" stringValue <* punctuation ',')
      (field "center" boolean)
    |])
  |]
  where
    build : Bounded String -> (String, Bool) -> Bounded SourceStatement
    build name (path, center) = atName name (GeometrySurface name path center)

geometryPolyhedronStatement : Rule True (Bounded SourceStatement)
geometryPolyhedronStatement = [| build
  identifier
  (punctuation '=' *> keyword "polyhedron" *>
    between (Symbol '(') (Symbol ')') [| MkPair
      (field "points" pointList3 <* punctuation ',')
      (field "faces" faceList)
    |])
  |]
  where
    build : Bounded String -> (List SourcePoint3, List (List Nat)) -> Bounded SourceStatement
    build name (points, faces) =
      atName name (GeometryPolyhedron name points faces)

geometryListStatement :
  String ->
  (Bounded String -> List (Bounded String) -> SourceStatement) ->
  Rule True (Bounded SourceStatement)
geometryListStatement kind constructor = [| build
  identifier
  (punctuation '=' *> keyword kind *> referenceList)
  |]
  where
    build : Bounded String -> List (Bounded String) -> Bounded SourceStatement
    build name references = atName name (constructor name references)

geometryDifferenceStatement : Rule True (Bounded SourceStatement)
geometryDifferenceStatement = [| build
  identifier
  (punctuation '=' *> keyword "difference" *> identifier)
  (keyword "by" *> referenceList)
  |]
  where
    build : Bounded String -> Bounded String -> List (Bounded String) -> Bounded SourceStatement
    build name base cutters = atName name (GeometryDifference name base cutters)

geometryBinaryStatement :
  String ->
  (Bounded String -> Bounded String -> SourceStatement) ->
  Rule True (Bounded SourceStatement)
geometryBinaryStatement kind constructor = [| build
  identifier
  (punctuation '=' *> keyword kind *> identifier)
  |]
  where
    build : Bounded String -> Bounded String -> Bounded SourceStatement
    build name child = atName name (constructor name child)

geometryMove2Statement : Rule True (Bounded SourceStatement)
geometryMove2Statement = [| build
  identifier
  (punctuation '=' *> keyword "move2" *> identifier)
  (keyword "by" *> vector2)
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Fixed) -> Bounded SourceStatement
    build name child (x, y) = atName name (GeometryMove2 name child x y)

geometryMove3Statement : Rule True (Bounded SourceStatement)
geometryMove3Statement = [| build
  identifier
  (punctuation '=' *> keyword "move" *> identifier)
  (keyword "by" *> vector3)
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Fixed, Fixed) -> Bounded SourceStatement
    build name child (x, y, z) = atName name (GeometryMove3 name child x y z)

geometryRotate2Statement : Rule True (Bounded SourceStatement)
geometryRotate2Statement = [| build
  identifier
  (punctuation '=' *> keyword "rotate2" *> identifier)
  (keyword "by" *> angle)
  |]
  where
    build : Bounded String -> Bounded String -> Fixed -> Bounded SourceStatement
    build name child rotation = atName name (GeometryRotate2 name child rotation)

geometryRotate3Statement : Rule True (Bounded SourceStatement)
geometryRotate3Statement = [| build
  identifier
  (punctuation '=' *> keyword "rotate" *> identifier)
  (keyword "by" *> angles3)
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Fixed, Fixed) -> Bounded SourceStatement
    build name child (x, y, z) = atName name (GeometryRotate3 name child x y z)

geometryScale2Statement : Rule True (Bounded SourceStatement)
geometryScale2Statement = [| build
  identifier
  (punctuation '=' *> keyword "scale2" *> identifier)
  (keyword "by" *> scale2)
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Fixed) -> Bounded SourceStatement
    build name child (x, y) = atName name (GeometryScale2 name child x y)

geometryScale3Statement : Rule True (Bounded SourceStatement)
geometryScale3Statement = [| build
  identifier
  (punctuation '=' *> keyword "scale" *> identifier)
  (keyword "by" *> scale3)
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Fixed, Fixed) -> Bounded SourceStatement
    build name child (x, y, z) = atName name (GeometryScale3 name child x y z)

geometryColourStatement : Rule True (Bounded SourceStatement)
geometryColourStatement = [| build
  identifier
  (punctuation '=' *> keyword "colour" *> identifier)
  stringValue
  |]
  where
    build : Bounded String -> Bounded String -> String -> Bounded SourceStatement
    build name child colour = atName name (GeometryColour name child colour)

geometryFacetsStatement : Rule True (Bounded SourceStatement)
geometryFacetsStatement = [| build
  identifier
  (punctuation '=' *> keyword "facets" *> identifier)
  natural
  |]
  where
    build : Bounded String -> Bounded String -> Nat -> Bounded SourceStatement
    build name child count = atName name (GeometryFacets name child count)

geometryExtrudeStatement : Rule True (Bounded SourceStatement)
geometryExtrudeStatement = [| build
  identifier
  (punctuation '=' *> keyword "extrude" *> identifier)
  (between (Symbol '(') (Symbol ')') [| MkPair
    (field "height" measurement <* punctuation ',')
    (field "center" boolean)
  |])
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Bool) -> Bounded SourceStatement
    build name child (height, center) =
      atName name (GeometryExtrude name child height center)

geometryTwistExtrudeStatement : Rule True (Bounded SourceStatement)
geometryTwistExtrudeStatement = [| build
  identifier
  (punctuation '=' *> keyword "twist_extrude" *> identifier)
  (between (Symbol '(') (Symbol ')') [| MkFields
    (field "height" measurement <* punctuation ',')
    (field "center" boolean <* punctuation ',')
    (field "twist" angle <* punctuation ',')
    (field "scale" scalar <* punctuation ',')
    (field "slices" natural)
  |])
  |]
  where
    MkFields : Fixed -> Bool -> Fixed -> Fixed -> Nat -> (Fixed, Bool, Fixed, Fixed, Nat)
    MkFields height center twist scaling slices =
      (height, center, twist, scaling, slices)
    build : Bounded String -> Bounded String -> (Fixed, Bool, Fixed, Fixed, Nat) -> Bounded SourceStatement
    build name child (height, center, twist, scaling, slices) =
      atName name (GeometryTwistExtrude name child height center twist scaling slices)

geometryRevolveStatement : Rule True (Bounded SourceStatement)
geometryRevolveStatement = [| build
  identifier
  (punctuation '=' *> keyword "revolve" *> identifier)
  (between (Symbol '(') (Symbol ')') [| MkPair
    (field "angle" angle <* punctuation ',')
    (field "convexity" natural)
  |])
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Nat) -> Bounded SourceStatement
    build name child (rotation, convexity) =
      atName name (GeometryRevolve name child rotation convexity)

geometryProjectionStatement : Rule True (Bounded SourceStatement)
geometryProjectionStatement = [| build
  identifier
  (punctuation '=' *> keyword "projection" *> identifier)
  (keyword "cut" *> boolean)
  |]
  where
    build : Bounded String -> Bounded String -> Bool -> Bounded SourceStatement
    build name child cut = atName name (GeometryProjection name child cut)

geometryOffsetStatement : Rule True (Bounded SourceStatement)
geometryOffsetStatement = [| build
  identifier
  (punctuation '=' *> keyword "offset" *> identifier)
  (between (Symbol '(') (Symbol ')') [| MkPair
    (field "delta" signedMeasurement <* punctuation ',')
    (field "chamfer" boolean)
  |])
  |]
  where
    build : Bounded String -> Bounded String -> (Fixed, Bool) -> Bounded SourceStatement
    build name child (delta, chamfer) =
      atName name (GeometryOffset name child delta chamfer)

geometryRoofStatement : Rule True (Bounded SourceStatement)
geometryRoofStatement = [| build
  identifier
  (punctuation '=' *> keyword "roof" *> identifier)
  (keyword "voronoi" *> boolean)
  |]
  where
    build : Bounded String -> Bounded String -> Bool -> Bounded SourceStatement
    build name child voronoi = atName name (GeometryRoof name child voronoi)

geometryRingStatement : Rule True (Bounded SourceStatement)
geometryRingStatement = [| build
  identifier
  (punctuation '=' *> keyword "ring" *> identifier)
  (between (Symbol '(') (Symbol ')') [| MkPair
    (field "count" natural <* punctuation ',')
    (field "radius" measurement)
  |])
  |]
  where
    build : Bounded String -> Bounded String -> (Nat, Fixed) -> Bounded SourceStatement
    build name child (count, radius) =
      atName name (GeometryRing name child count radius)

emitSolidStatement : Rule True (Bounded SourceStatement)
emitSolidStatement = [| build (keyword "solid" *> identifier) |]
  where
    build : Bounded String -> Bounded SourceStatement
    build reference = atName reference (EmitSolid reference)

requireStatement : Rule True (Bounded SourceStatement)
requireStatement = [| build
  (keyword "require" *> measurement)
  (keyword "at_least" *> measurement)
  (keyword "because" *> stringValue)
  |]
  where
    build : Fixed -> Fixed -> String -> Bounded SourceStatement
    build actual minimum message = B (RequireAtLeast actual minimum message) NoBounds

geometryTreeStatement : Rule True (Bounded SourceStatement)
geometryTreeStatement = [| build
  identifier
  (punctuation '=' *> keyword "branching_tree" *>
    between (Symbol '(') (Symbol ')') [| MkFields
      (field "levels" natural <* punctuation ',')
      (field "length" measurement <* punctuation ',')
      (field "thickness" measurement)
    |])
  |]
  where
    MkFields : Nat -> Fixed -> Fixed -> (Nat, Fixed, Fixed)
    MkFields levels length thickness = (levels, length, thickness)
    build : Bounded String -> (Nat, Fixed, Fixed) -> Bounded SourceStatement
    build name (levels, length, thickness) =
      atName name (GeometryTree name levels length thickness)

data StatementKind
  = PlateKind
  | CutoutKind
  | BoreKind
  | CornerKind
  | CenterKind
  | AlignXKind
  | AlignYKind
  | LeftKind
  | RightKind
  | AboveKind
  | BelowKind
  | ColumnsKind
  | RowsKind
  | SpaceKind
  | MinimizeKind
  | BoxKind | RoundedBoxKind | SphereKind | CylinderKind
  | SquareKind | CircleKind | PolygonKind | RegularPolygonKind | StarKind
  | TextKind | Import2DKind | Import3DKind | SurfaceKind | PolyhedronKind
  | UnionKind | DifferenceKind | IntersectionKind | HullKind
  | Move2Kind | Move3Kind | Rotate2Kind | Rotate3Kind
  | Scale2Kind | Scale3Kind | ColourKind | HighlightKind | BackgroundKind
  | FacetsKind | ExtrudeKind | TwistExtrudeKind | RevolveKind
  | ProjectionKind | OffsetKind | RoofKind | RingKind | TreeKind
  | SolidKind | RequireKind
  | UnknownKind

statementKind : List (Bounded Token) -> StatementKind
statementKind (B (Word "solid") _ :: rest) = SolidKind
statementKind (B (Word "require") _ :: rest) = RequireKind
statementKind
    (B (Word name) _ :: B (Symbol '=') _ :: B (Word kind) _ :: rest) =
  geometryKind kind
  where
    geometryKind : String -> StatementKind
    geometryKind "plate" = PlateKind
    geometryKind "cutout" = CutoutKind
    geometryKind "bore" = BoreKind
    geometryKind "corner_bores" = CornerKind
    geometryKind "box" = BoxKind
    geometryKind "rounded_box" = RoundedBoxKind
    geometryKind "sphere" = SphereKind
    geometryKind "cylinder" = CylinderKind
    geometryKind "rectangle" = SquareKind
    geometryKind "circle" = CircleKind
    geometryKind "polygon" = PolygonKind
    geometryKind "regular_polygon" = RegularPolygonKind
    geometryKind "star" = StarKind
    geometryKind "text" = TextKind
    geometryKind "import2d" = Import2DKind
    geometryKind "import3d" = Import3DKind
    geometryKind "surface" = SurfaceKind
    geometryKind "polyhedron" = PolyhedronKind
    geometryKind "union" = UnionKind
    geometryKind "difference" = DifferenceKind
    geometryKind "intersection" = IntersectionKind
    geometryKind "hull" = HullKind
    geometryKind "move2" = Move2Kind
    geometryKind "move" = Move3Kind
    geometryKind "rotate2" = Rotate2Kind
    geometryKind "rotate" = Rotate3Kind
    geometryKind "scale2" = Scale2Kind
    geometryKind "scale" = Scale3Kind
    geometryKind "colour" = ColourKind
    geometryKind "highlight" = HighlightKind
    geometryKind "background" = BackgroundKind
    geometryKind "facets" = FacetsKind
    geometryKind "extrude" = ExtrudeKind
    geometryKind "twist_extrude" = TwistExtrudeKind
    geometryKind "revolve" = RevolveKind
    geometryKind "projection" = ProjectionKind
    geometryKind "offset" = OffsetKind
    geometryKind "roof" = RoofKind
    geometryKind "ring" = RingKind
    geometryKind "branching_tree" = TreeKind
    geometryKind kind = UnknownKind
statementKind (B (Word "center") _ :: rest) = CenterKind
statementKind (B (Word "align_x") _ :: rest) = AlignXKind
statementKind (B (Word "align_y") _ :: rest) = AlignYKind
statementKind (B (Word "between_columns") _ :: rest) = ColumnsKind
statementKind (B (Word "between_rows") _ :: rest) = RowsKind
statementKind (B (Word "space") _ :: rest) = SpaceKind
statementKind (B (Word "minimize") _ :: rest) = MinimizeKind
statementKind (B (Word subject) _ :: B (Word "left_of") _ :: rest) = LeftKind
statementKind (B (Word subject) _ :: B (Word "right_of") _ :: rest) = RightKind
statementKind (B (Word subject) _ :: B (Word "above") _ :: rest) = AboveKind
statementKind (B (Word subject) _ :: B (Word "below") _ :: rest) = BelowKind
statementKind tokens = UnknownKind

statement : Rule True (Bounded SourceStatement)
statement [] = eoi
statement tokens = case statementKind tokens of
  PlateKind => plateStatement tokens
  CutoutKind => cutoutStatement tokens
  BoreKind => boreStatement tokens
  CornerKind => cornerStatement tokens
  CenterKind => centerStatement tokens
  AlignXKind => alignXStatement tokens
  AlignYKind => alignYStatement tokens
  LeftKind => directionalStatement "left_of" LeftOf tokens
  RightKind => directionalStatement "right_of" RightOf tokens
  AboveKind => directionalStatement "above" Above tokens
  BelowKind => directionalStatement "below" Below tokens
  ColumnsKind => betweenColumnsStatement tokens
  RowsKind => betweenRowsStatement tokens
  SpaceKind => spaceStatement tokens
  MinimizeKind => minimizeStatement tokens
  BoxKind => geometryBoxStatement tokens
  RoundedBoxKind => geometryRoundedBoxStatement tokens
  SphereKind => geometrySphereStatement tokens
  CylinderKind => geometryCylinderStatement tokens
  SquareKind => geometrySquareStatement tokens
  CircleKind => geometryCircleStatement tokens
  PolygonKind => geometryPolygonStatement tokens
  RegularPolygonKind => geometryRegularPolygonStatement tokens
  StarKind => geometryStarStatement tokens
  TextKind => geometryTextStatement tokens
  Import2DKind => geometryImportStatement "import2d" False tokens
  Import3DKind => geometryImportStatement "import3d" True tokens
  SurfaceKind => geometrySurfaceStatement tokens
  PolyhedronKind => geometryPolyhedronStatement tokens
  UnionKind => geometryListStatement "union" GeometryUnion tokens
  DifferenceKind => geometryDifferenceStatement tokens
  IntersectionKind => geometryListStatement "intersection" GeometryIntersection tokens
  HullKind => geometryListStatement "hull" GeometryHull tokens
  Move2Kind => geometryMove2Statement tokens
  Move3Kind => geometryMove3Statement tokens
  Rotate2Kind => geometryRotate2Statement tokens
  Rotate3Kind => geometryRotate3Statement tokens
  Scale2Kind => geometryScale2Statement tokens
  Scale3Kind => geometryScale3Statement tokens
  ColourKind => geometryColourStatement tokens
  HighlightKind => geometryBinaryStatement "highlight" GeometryHighlight tokens
  BackgroundKind => geometryBinaryStatement "background" GeometryBackground tokens
  FacetsKind => geometryFacetsStatement tokens
  ExtrudeKind => geometryExtrudeStatement tokens
  TwistExtrudeKind => geometryTwistExtrudeStatement tokens
  RevolveKind => geometryRevolveStatement tokens
  ProjectionKind => geometryProjectionStatement tokens
  OffsetKind => geometryOffsetStatement tokens
  RoofKind => geometryRoofStatement tokens
  RingKind => geometryRingStatement tokens
  TreeKind => geometryTreeStatement tokens
  SolidKind => emitSolidStatement tokens
  RequireKind => requireStatement tokens
  UnknownKind => fail tokens

fatalStatementError : InnerError SourceParseIssue -> Bool
fatalStatementError EOI = False
fatalStatementError problem = True

stripBounds : Bounded SourceStatement -> SourceStatement
stripBounds = val

stripStatements : List (Bounded SourceStatement) -> List SourceStatement
stripStatements = map stripBounds

program : Rule True SourceProgram
program = [| MkSourceProgram
  (keyword "model" *> identifier)
  ([| stripStatements (manyF fatalStatementError statement) |])
  |]

public export
parseSource :
  Origin -> String -> Either (ParseError SourceParseIssue) SourceProgram
parseSource origin source = case lexSource source of
  Left problem => Left $
    toParseError origin source (map fromVoid problem)
  Right tokens => result origin source (program tokens)
