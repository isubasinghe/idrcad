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

||| An axis-aligned rectangular footprint. Coordinates identify its lower-left
||| corner, which keeps containment and non-overlap constraints integer-linear.
public export
record Footprint2D where
  constructor Footprint
  footprintX : Expr
  footprintY : Expr
  footprintWidth : Expr
  footprintDepth : Expr

||| A scalar relation, or a native two-dimensional packing constraint. Keeping
||| non-overlap as one constraint lets MiniZinc choose the separating axis.
public export
data Constraint
  = Constrain Expr Relation Expr String
  | NonOverlapping (List Footprint2D) String

evaluatedFootprint :
  Environment ->
  Footprint2D ->
  Maybe (Integer, Integer, Integer, Integer)
evaluatedFootprint environment (Footprint x y width depth) =
  case (evaluate environment x, evaluate environment y,
        evaluate environment width, evaluate environment depth) of
    (Just (MkFixed xValue), Just (MkFixed yValue),
     Just (MkFixed widthValue), Just (MkFixed depthValue)) =>
      Just (xValue, yValue, widthValue, depthValue)
    _ => Nothing

validFootprint : Environment -> Footprint2D -> Bool
validFootprint environment footprint =
  case evaluatedFootprint environment footprint of
    Just (_, _, width, depth) => width > 0 && depth > 0
    Nothing => False

allValidFootprints : Environment -> List Footprint2D -> Bool
allValidFootprints environment [] = True
allValidFootprints environment (footprint :: rest) =
  validFootprint environment footprint
    && allValidFootprints environment rest

doNotOverlap : Environment -> Footprint2D -> Footprint2D -> Bool
doNotOverlap environment left right =
  case (evaluatedFootprint environment left,
        evaluatedFootprint environment right) of
    (Just (leftX, leftY, leftWidth, leftDepth),
     Just (rightX, rightY, rightWidth, rightDepth)) =>
      leftX + leftWidth <= rightX
        || rightX + rightWidth <= leftX
        || leftY + leftDepth <= rightY
        || rightY + rightDepth <= leftY
    _ => False

separateFromAll :
  Environment -> Footprint2D -> List Footprint2D -> Bool
separateFromAll environment footprint [] = True
separateFromAll environment footprint (candidate :: rest) =
  doNotOverlap environment footprint candidate
    && separateFromAll environment footprint rest

pairwiseNonOverlapping : Environment -> List Footprint2D -> Bool
pairwiseNonOverlapping environment [] = True
pairwiseNonOverlapping environment (footprint :: rest) =
  separateFromAll environment footprint rest
    && pairwiseNonOverlapping environment rest

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
satisfies tolerance environment (NonOverlapping footprints message) =
  allValidFootprints environment footprints
    && pairwiseNonOverlapping environment footprints

public export
validate : (equalityTolerance : Fixed) -> Environment -> List Constraint -> Bool
validate tolerance environment [] = True
validate tolerance environment (constraint :: rest) =
  satisfies tolerance environment constraint && validate tolerance environment rest

public export
isSolverConstraint : Constraint -> Bool
isSolverConstraint (Constrain left relation right message) =
  isIntegerLinear left && isIntegerLinear right
isSolverConstraint (NonOverlapping footprints message) =
  solverFootprints footprints
  where
    solverFootprint : Footprint2D -> Bool
    solverFootprint (Footprint x y width depth) =
      isIntegerLinear x
        && isIntegerLinear y
        && isIntegerLinear width
        && isIntegerLinear depth

    solverFootprints : List Footprint2D -> Bool
    solverFootprints [] = True
    solverFootprints (footprint :: rest) =
      solverFootprint footprint && solverFootprints rest

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
