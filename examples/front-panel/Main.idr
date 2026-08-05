module Main

import Idrcad.Backend.MiniZinc
import Idrcad.Backend.OpenSCAD
import Idrcad.Examples.FrontPanel
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System

%default total

emitMiniZinc : IO ()
emitMiniZinc =
  case renderMiniZinc frontPanelModel of
    Right source => putStr source
    Left problem => do
      putStrLn ("Cannot lower front panel to MiniZinc: " ++ problem)
      exitFailure

covering
emitSolved : IO ()
emitSolved = do
  Right environment <- solve frontPanelModel
    | Left problem => do
        putStrLn ("Could not solve front panel: " ++ show problem)
        exitFailure
  putStr (renderModelWith environment frontPanelModel)

covering
main : IO ()
main = do
  arguments <- getArgs
  case arguments of
    [_] => emitSolved
    [_, "--defaults"] => putStr (renderModel frontPanelModel)
    [_, "--minizinc"] => emitMiniZinc
    _ => do
      putStrLn "Usage: front-panel [--defaults | --minizinc]"
      exitFailure
