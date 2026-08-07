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

public export
Eq SourceParseIssue where
  InvalidMeasurement left == InvalidMeasurement right = left == right

public export
Interpolation SourceParseIssue where
  interpolate (InvalidMeasurement value) =
    "Invalid exact millimetre measurement: " ++ value
      ++ ". Use at most six decimal places."

data Token
  = Word String
  | Number String
  | Symbol Char
  | DotDot
  | Ignored

Eq Token where
  Word left == Word right = left == right
  Number left == Number right = left == right
  Symbol left == Symbol right = left == right
  DotDot == DotDot = True
  Ignored == Ignored = True
  _ == _ = False

Interpolation Token where
  interpolate (Word value) = value
  interpolate (Number value) = value
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

term : Tok True lexError Token
term ('=' :: rest) = Succ (Symbol '=') rest
term ('(' :: rest) = Succ (Symbol '(') rest
term (')' :: rest) = Succ (Symbol ')') rest
term (',' :: rest) = Succ (Symbol ',') rest
term ('[' :: rest) = Succ (Symbol '[') rest
term (']' :: rest) = Succ (Symbol ']') rest
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
  | UnknownKind

statementKind : List (Bounded Token) -> StatementKind
statementKind
    (B (Word name) _ :: B (Symbol '=') _ :: B (Word "plate") _ :: rest) =
  PlateKind
statementKind
    (B (Word name) _ :: B (Symbol '=') _ :: B (Word "cutout") _ :: rest) =
  CutoutKind
statementKind
    (B (Word name) _ :: B (Symbol '=') _ :: B (Word "bore") _ :: rest) =
  BoreKind
statementKind
    (B (Word name) _ :: B (Symbol '=') _ ::
     B (Word "corner_bores") _ :: rest) = CornerKind
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
