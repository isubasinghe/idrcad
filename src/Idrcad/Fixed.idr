module Idrcad.Fixed

%default total

||| Exact fixed-point scalar with six decimal places. The stored value is an
||| arbitrary-precision integer; no floating-point value enters the model.
public export
record Fixed where
  constructor MkFixed
  raw : Integer

||| Number of integer ticks representing one whole unit.
public export
fixedScale : Integer
fixedScale = 1000000

public export
whole : Integer -> Fixed
whole value = MkFixed (value * fixedScale)

public export
tenths : Integer -> Fixed
tenths value = MkFixed (value * 100000)

public export
hundredths : Integer -> Fixed
hundredths value = MkFixed (value * 10000)

public export
thousandths : Integer -> Fixed
thousandths value = MkFixed (value * 1000)

public export
millionths : Integer -> Fixed
millionths = MkFixed

public export
addFixed : Fixed -> Fixed -> Fixed
addFixed (MkFixed left) (MkFixed right) = MkFixed (left + right)

public export
subtractFixed : Fixed -> Fixed -> Fixed
subtractFixed (MkFixed left) (MkFixed right) = MkFixed (left - right)

public export
negateFixed : Fixed -> Fixed
negateFixed (MkFixed value) = MkFixed (0 - value)

||| Fixed-point multiplication succeeds only when the result is exactly
||| representable at the configured resolution.
public export
multiplyExact : Fixed -> Fixed -> Maybe Fixed
multiplyExact (MkFixed left) (MkFixed right) =
  let product = left * right
   in if product `mod` fixedScale == 0
        then Just (MkFixed (product `div` fixedScale))
        else Nothing

||| Fixed-point division succeeds only for non-zero divisors and results that
||| are exactly representable at the configured resolution.
public export
divideExact : Fixed -> Fixed -> Maybe Fixed
divideExact (MkFixed left) (MkFixed right) =
  if right == 0
    then Nothing
    else
      let numerator = left * fixedScale
       in if numerator `mod` right == 0
            then Just (MkFixed (numerator `div` right))
            else Nothing

public export
absoluteDifference : Fixed -> Fixed -> Fixed
absoluteDifference (MkFixed left) (MkFixed right) =
  MkFixed (abs (left - right))

zeros : Nat -> String
zeros Z = ""
zeros (S count) = "0" ++ zeros count

padSix : String -> String
padSix digits = zeros (minus 6 (length (unpack digits))) ++ digits

renderPositive : Integer -> String
renderPositive value =
  let units = value `div` fixedScale
      fraction = value `mod` fixedScale
   in show units ++ "." ++ padSix (show fraction)

||| Render exactly as an OpenSCAD decimal without passing through Double.
public export
renderFixed : Fixed -> String
renderFixed (MkFixed value) =
  if value < 0
    then "-" ++ renderPositive (0 - value)
    else renderPositive value
