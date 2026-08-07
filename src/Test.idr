module Test

import Data.String
import Idrcad.Backend.OpenSCAD
import Idrcad.Backend.MiniZinc
import Idrcad.Constraint
import Idrcad.Examples.Basics
import Idrcad.Examples.FrontPanel
import Idrcad.Examples.PartialFit
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Language.Compiler
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System
import System.File
import Text.FC

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

semanticFailure : Either LanguageError (Model ThreeD) -> Bool
semanticFailure (Left (SemanticFailure problem)) = True
semanticFailure result = False

parseFailure : Either LanguageError (Model ThreeD) -> Bool
parseFailure (Left (ParseFailure problem)) = True
parseFailure result = False

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
  sourceResult <- readFile "examples/front-panel/front-panel.idrcad"
  case sourceResult of
    Left problem => check "textual front panel source is readable" False
    Right source => case compileSource
        (FileSrc "examples/front-panel/front-panel.idrcad") source of
      Left problem => check "textual front panel parses and elaborates" False
      Right languageModel => do
        check "textual front panel elaborates into the integer solver fragment"
          (length languageModel.modelParameters == 8
            && allSolverConstraints languageModel.modelConstraints)
        solvedLanguage <- solve languageModel
        check "textual and Idris front panels derive the same layout" $
          case solvedLanguage of
            Right environment =>
              solutionIsValid environment languageModel
                && bindingEquals "width_0" 116900000 environment
                && bindingEquals "depth_1" 85700000 environment
                && bindingEquals "x_2" 58450000 environment
                && bindingEquals "y_3" 42850000 environment
                && bindingEquals "x_6" 100700000 environment
                && bindingEquals "y_5" 15900000 environment
            Left problem => False
  check "unknown textual feature references fail during elaboration" $
    semanticFailure $ compileSource Virtual $ unlines
      [ "model broken"
      , "panel = plate(width = 10mm, depth = 10mm, height = 2mm)"
      , "center missing in panel"
      ]
  check "over-precise decimal measurements fail during parsing" $
    parseFailure $ compileSource Virtual $ unlines
      [ "model broken"
      , "panel = plate(width = 10mm, depth = 10mm, height = 0.0000001mm)"
      ]
  check "textual CSG elaborates through the dimension-indexed geometry IR" $
    case compileSource Virtual $ unlines
      [ "model csg"
      , "cube = box(width = 15mm, depth = 15mm, height = 15mm, center = true)"
      , "ball = sphere(radius = 10mm)"
      , "result = difference cube by [ball]"
      , "solid result"
      ] of
        Right model => length (unpack (renderModel model)) > 20
        Left problem => False
  check "textual CSG rejects mixed 2D and 3D operands" $
    semanticFailure $ compileSource Virtual $ unlines
      [ "model broken"
      , "profile = circle(radius = 10mm)"
      , "ball = sphere(radius = 10mm)"
      , "bad = union [profile, ball]"
      , "solid ball"
      ]
