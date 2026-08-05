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

covering
main : IO ()
main = do
  check "all eleven OpenSCAD Basics models are represented" (length basics == 11)
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
          && bindingEquals "panel_width" 116900000 environment
          && bindingEquals "panel_depth" 85700000 environment
          && bindingEquals "display_x" 58450000 environment
          && bindingEquals "display_y" 42850000 environment
          && bindingEquals "encoder_x" 100700000 environment
          && bindingEquals "usb_y" 15900000 environment
      Left problem => False
