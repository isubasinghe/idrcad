module Idrcad.Expr

import Idrcad.Fixed

%default total

||| Symbolic scalar expressions shared by geometry and constraints.
public export
data Expr
  = Lit Fixed
  | Var String
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Negate Expr
  | Modulo Expr Expr
  | Power Expr Expr
  | Sine Expr
  | Cosine Expr
  | ArcCosine Expr
  | ArcTangent2 Expr Expr
  | SquareRoot Expr
  | Floor Expr

public export
literal : Fixed -> Expr
literal = Lit

public export
integer : Integer -> Expr
integer = Lit . whole

public export
variable : String -> Expr
variable = Var

public export
add : Expr -> Expr -> Expr
add = Add

public export
subtract : Expr -> Expr -> Expr
subtract = Subtract

public export
multiply : Expr -> Expr -> Expr
multiply = Multiply

public export
divide : Expr -> Expr -> Expr
divide = Divide

public export
negate : Expr -> Expr
negate = Negate

||| Geometry-only symbolic operations are preserved for OpenSCAD. They are
||| deliberately outside the exact integer solver fragment.
public export
modulo : Expr -> Expr -> Expr
modulo = Modulo

public export
power : Expr -> Expr -> Expr
power = Power

public export
sine : Expr -> Expr
sine = Sine

public export
cosine : Expr -> Expr
cosine = Cosine

public export
arcCosine : Expr -> Expr
arcCosine = ArcCosine

public export
arcTangent2 : Expr -> Expr -> Expr
arcTangent2 = ArcTangent2

public export
squareRoot : Expr -> Expr
squareRoot = SquareRoot

public export
floorExpr : Expr -> Expr
floorExpr = Floor

public export
Environment : Type
Environment = List (String, Fixed)

lookupValue : String -> Environment -> Maybe Fixed
lookupValue name [] = Nothing
lookupValue name ((candidate, value) :: rest) =
  if name == candidate then Just value else lookupValue name rest

||| Evaluate an expression after assigning all of its variables.
public export
evaluate : Environment -> Expr -> Maybe Fixed
evaluate environment (Lit value) = Just value
evaluate environment (Var name) = lookupValue name environment
evaluate environment (Add left right) =
  case (evaluate environment left, evaluate environment right) of
    (Just x, Just y) => Just (addFixed x y)
    _ => Nothing
evaluate environment (Subtract left right) =
  case (evaluate environment left, evaluate environment right) of
    (Just x, Just y) => Just (subtractFixed x y)
    _ => Nothing
evaluate environment (Multiply left right) =
  case (evaluate environment left, evaluate environment right) of
    (Just x, Just y) => multiplyExact x y
    _ => Nothing
evaluate environment (Divide left right) =
  case (evaluate environment left, evaluate environment right) of
    (Just x, Just y) => divideExact x y
    _ => Nothing
evaluate environment (Negate expression) =
  map negateFixed (evaluate environment expression)
evaluate environment (Modulo left right) = Nothing
evaluate environment (Power base exponent) = Nothing
evaluate environment (Sine angle) = Nothing
evaluate environment (Cosine angle) = Nothing
evaluate environment (ArcCosine value) = Nothing
evaluate environment (ArcTangent2 y x) = Nothing
evaluate environment (SquareRoot value) = Nothing
evaluate environment (Floor value) = Nothing

||| Whether an expression is in the integer-linear fragment accepted by the
||| MiniZinc/CP-SAT backend. Multiplication is allowed only by whole constants;
||| division and products of variables are deliberately excluded.
public export
isIntegerLinear : Expr -> Bool
isIntegerLinear (Lit value) = True
isIntegerLinear (Var name) = True
isIntegerLinear (Add left right) = isIntegerLinear left && isIntegerLinear right
isIntegerLinear (Subtract left right) =
  isIntegerLinear left && isIntegerLinear right
isIntegerLinear (Multiply (Lit (MkFixed coefficient)) expression) =
  coefficient `mod` fixedScale == 0 && isIntegerLinear expression
isIntegerLinear (Multiply expression (Lit (MkFixed coefficient))) =
  coefficient `mod` fixedScale == 0 && isIntegerLinear expression
isIntegerLinear (Multiply left right) = False
isIntegerLinear (Divide left right) = False
isIntegerLinear (Negate expression) = isIntegerLinear expression
isIntegerLinear (Modulo left right) = False
isIntegerLinear (Power base exponent) = False
isIntegerLinear (Sine angle) = False
isIntegerLinear (Cosine angle) = False
isIntegerLinear (ArcCosine value) = False
isIntegerLinear (ArcTangent2 y x) = False
isIntegerLinear (SquareRoot value) = False
isIntegerLinear (Floor value) = False
