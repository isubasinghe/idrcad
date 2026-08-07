module Test

import Idrcad.Backend.OpenSCAD
import Idrcad.Backend.MiniZinc
import Idrcad.Constraint
import Idrcad.Examples.Basics
import Idrcad.Examples.FrontPanel
import Idrcad.Examples.PartialFit
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System

check : String -> Bool -> IO ()
check label True = putStrLn ("PASS: " ++ label)
check label False = do
  putStrLn ("FAIL: " ++ label)
  exitFailure

allRender : List (String, Model ThreeD) -> Bool
allRender [] = True
allRender ((name, model) :: rest) =
  length (unpack (renderModel model)) > 20 && allRender rest

makeFitInvalid : (String, Fixed) -> (String, Fixed)
makeFitInvalid ("pin_radius", value) = ("pin_radius", whole 11)
makeFitInvalid binding = binding

bindingEquals : String -> Integer -> Environment -> Bool
bindingEquals requested expected [] = False
bindingEquals requested expected ((name, MkFixed value) :: rest) =
  if requested == name
    then value == expected
    else bindingEquals requested expected rest

box : Integer -> Integer -> Integer -> Integer -> Footprint2D
box x y width depth = Footprint
  (integer x) (integer y) (integer width) (integer depth)

covering
main : IO ()
main = do
  check "all eleven OpenSCAD Basics models are represented" (length basics == 11)
  check "all fifty upstream OpenSCAD examples are represented"
    (length upstreamSourcePaths == 50 && length upstreamExamples == 50)
  check "catalog contains three idrcad designs plus fifty upstream ports"
    (length examples == 53)
  check "every model renders non-empty OpenSCAD" (allRender examples)
  check "default clearance-fit parameters satisfy their constraints"
    (defaultsAreValid (millionths 1) constrainedFitModel)
  check "an oversized pin violates the clearance-fit constraints"
    (not (validate
      (millionths 1)
      (map makeFitInvalid (defaultEnvironment constrainedFitModel))
      constrainedFitModel.modelConstraints))
  check "all fitting constraints are in the CP-SAT integer-linear fragment"
    (allSolverConstraints constrainedFitModel.modelConstraints)
  check "native 2D packing accepts touching boxes and rejects overlap"
    (satisfies (millionths 0) []
      (NonOverlapping [box 0 0 10 10, box 10 0 5 5] "touching")
      && not (satisfies (millionths 0) []
        (NonOverlapping [box 0 0 10 10, box 9 0 5 5] "overlap")))
  check "the fitting model lowers to MiniZinc without floats" $
    case renderMiniZinc constrainedFitModel of
      Right source => length (unpack source) > 20
      Left problem => False
  check "unknown example lookup is rejected" $
    case findExample "missing" of
      Nothing => True
      Just model => False
  solvedPartialFit <- solve partialFitModel
  check "idrcad invokes MiniZinc and accepts its checked optimal solution" $
    case solvedPartialFit of
      Right environment =>
        solutionIsValid environment partialFitModel
          && bindingEquals "pin_radius" 9700000 environment
      Left problem => False
  solvedFrontPanel <- solve frontPanelModel
  check "front panel size and positions are derived by MiniZinc" $
    case solvedFrontPanel of
      Right environment =>
        solutionIsValid environment frontPanelModel
          && bindingEquals "width_0" 116900000 environment
          && bindingEquals "depth_1" 85700000 environment
          && bindingEquals "x_2" 58450000 environment
          && bindingEquals "y_3" 42850000 environment
          && bindingEquals "x_6" 100700000 environment
          && bindingEquals "y_5" 15900000 environment
      Left problem => False
