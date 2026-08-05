module Main

import Idrcad.Backend.OpenSCAD
import Idrcad.Backend.MiniZinc
import Idrcad.Examples.Basics
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System

joinWith : String -> List String -> String
joinWith separator [] = ""
joinWith separator [value] = value
joinWith separator (value :: rest) = value ++ separator ++ joinWith separator rest

exampleNames : List String
exampleNames = map fst examples

usage : String
usage = "Usage: idrcad [--list | EXAMPLE | --solve EXAMPLE | --minizinc EXAMPLE]\n\nExamples:\n  "
  ++ joinWith "\n  " exampleNames

emit : String -> IO ()
emit requested =
  case findExample requested of
    Just model => putStr (renderModel model)
    Nothing => do
      putStrLn ("Unknown example: " ++ requested)
      putStrLn usage
      exitFailure

emitMiniZinc : String -> IO ()
emitMiniZinc requested =
  case findExample requested of
    Nothing => do
      putStrLn ("Unknown example: " ++ requested)
      putStrLn usage
      exitFailure
    Just model =>
      case renderMiniZinc model of
        Right source => putStr source
        Left problem => do
          putStrLn ("Cannot lower model to MiniZinc: " ++ problem)
          exitFailure

covering
emitSolved : String -> IO ()
emitSolved requested =
  case findExample requested of
    Nothing => do
      putStrLn ("Unknown example: " ++ requested)
      putStrLn usage
      exitFailure
    Just model => do
      Right environment <- solve model
        | Left problem => do
            putStrLn ("Cannot solve model: " ++ show problem)
            exitFailure
      putStr (renderModelWith environment model)

covering
main : IO ()
main = do
  arguments <- getArgs
  case arguments of
    [_] => emit "constrained-fit"
    [_, "--list"] => traverse_ putStrLn exampleNames
    [_, "--solve", requested] => emitSolved requested
    [_, "--minizinc", requested] => emitMiniZinc requested
    [_, requested] => emit requested
    _ => do
      putStrLn usage
      exitFailure
