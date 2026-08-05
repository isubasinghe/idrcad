module Idrcad.Constraint

import Idrcad.Expr
import Idrcad.Fixed
import Data.Nat

%default total

public export
data Relation
  = Equal
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

||| A symbolic relation with a useful failure message for generated OpenSCAD.
public export
record Constraint where
  constructor Constrain
  left : Expr
  relation : Relation
  right : Expr
  message : String

public export
satisfies : (equalityTolerance : Fixed) -> Environment -> Constraint -> Bool
satisfies tolerance environment (Constrain left Equal right message) =
  case (evaluate environment left, evaluate environment right) of
    (Just (MkFixed x), Just (MkFixed y)) =>
      let MkFixed allowed = tolerance
       in abs (x - y) <= allowed
    _ => False
satisfies tolerance environment (Constrain left LessThan right message) =
  case (evaluate environment left, evaluate environment right) of
    (Just (MkFixed x), Just (MkFixed y)) => x < y
    _ => False
satisfies tolerance environment (Constrain left LessOrEqual right message) =
  case (evaluate environment left, evaluate environment right) of
    (Just (MkFixed x), Just (MkFixed y)) => x <= y
    _ => False
satisfies tolerance environment (Constrain left GreaterThan right message) =
  case (evaluate environment left, evaluate environment right) of
    (Just (MkFixed x), Just (MkFixed y)) => x > y
    _ => False
satisfies tolerance environment (Constrain left GreaterOrEqual right message) =
  case (evaluate environment left, evaluate environment right) of
    (Just (MkFixed x), Just (MkFixed y)) => x >= y
    _ => False

public export
validate : (equalityTolerance : Fixed) -> Environment -> List Constraint -> Bool
validate tolerance environment [] = True
validate tolerance environment (constraint :: rest) =
  satisfies tolerance environment constraint && validate tolerance environment rest

public export
isSolverConstraint : Constraint -> Bool
isSolverConstraint (Constrain left relation right message) =
  isIntegerLinear left && isIntegerLinear right

public export
allSolverConstraints : List Constraint -> Bool
allSolverConstraints [] = True
allSolverConstraints (constraint :: rest) =
  isSolverConstraint constraint && allSolverConstraints rest

||| Compile-time evidence that a pin plus its clearance fits inside a hole.
||| Natural-number dimensions can represent exact units such as micrometres.
public export
record ClearanceFit
    (pinRadius : Nat)
    (holeRadius : Nat)
    (minimumClearance : Nat) where
  constructor MkClearanceFit
  0 fits : LTE (pinRadius + minimumClearance) holeRadius

||| Worst-case manufacturing fit:
||| max(pin) + clearance <= min(hole), rearranged to avoid subtraction.
public export
record TolerancedFit
    (pinNominal : Nat)
    (pinTolerance : Nat)
    (holeNominal : Nat)
    (holeTolerance : Nat)
    (minimumClearance : Nat) where
  constructor MkTolerancedFit
  0 guaranteed : LTE
    (pinNominal + pinTolerance + minimumClearance + holeTolerance)
    holeNominal
