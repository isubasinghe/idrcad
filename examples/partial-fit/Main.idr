module Main

import Idrcad.Backend.MiniZinc
import Idrcad.Backend.OpenSCAD
import Idrcad.Examples.PartialFit
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System

%default total

emitMiniZinc : IO ()
emitMiniZinc =
  case renderMiniZinc partialFitModel of
    Right source => putStr source
    Left problem => do
      putStrLn ("Cannot lower partial fit to MiniZinc: " ++ problem)
      exitFailure

covering
emitSolved : IO ()
emitSolved = do
  Right environment <- solve partialFitModel
    | Left problem => do
        putStrLn ("Could not solve partial fit: " ++ show problem)
        exitFailure
  putStr (renderModelWith environment partialFitModel)

covering
main : IO ()
main = do
  arguments <- getArgs
  case arguments of
    [_] => emitSolved
    [_, "--defaults"] => putStr (renderModel partialFitModel)
    [_, "--minizinc"] => emitMiniZinc
    _ => do
      putStrLn "Usage: partial-fit [--defaults | --minizinc]"
      exitFailure
