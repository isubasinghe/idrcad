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
