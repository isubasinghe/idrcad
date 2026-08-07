module Main

import Idrcad.Backend.OpenSCAD
import Idrcad.Backend.MiniZinc
import Idrcad.Constraint
import Idrcad.Examples.Basics
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Language.Compiler
import Idrcad.Model
import Idrcad.Solver.MiniZinc
import System
import System.File
import Text.FC

joinWith : String -> List String -> String
joinWith separator [] = ""
joinWith separator [value] = value
joinWith separator (value :: rest) = value ++ separator ++ joinWith separator rest

exampleNames : List String
exampleNames = map fst examples

usage : String
usage = "Usage: idrcad [--list | EXAMPLE | --solve EXAMPLE | --minizinc EXAMPLE\n"
  ++ "              | check FILE.idrcad | build FILE.idrcad | minizinc FILE.idrcad]\n\n"
  ++ "Language commands:\n"
  ++ "  idrcad check model.idrcad\n"
  ++ "  idrcad build model.idrcad > model.scad\n\n"
  ++ "Bundled examples:\n  "
  ++ joinWith "\n  " exampleNames

covering
withLanguageModel : String -> (Model ThreeD -> IO ()) -> IO ()
withLanguageModel path continue = do
  Right source <- readFile path
    | Left problem => do
        putStrLn ("Cannot read " ++ path ++ ": " ++ show problem)
        exitFailure
  case compileSource (FileSrc path) source of
    Left problem => do
      putStrLn (interpolate problem)
      exitFailure
    Right model => continue model

covering
checkLanguage : String -> IO ()
checkLanguage path = withLanguageModel path $ \model =>
  putStrLn $ "OK: " ++ model.modelName ++ " ("
    ++ show (length model.modelParameters) ++ " solver variables, "
    ++ show (length model.modelConstraints) ++ " constraints)"

covering
buildLanguage : String -> IO ()
buildLanguage path = withLanguageModel path $ \model => do
  Right environment <- solve model
    | Left problem => do
        putStrLn ("Cannot solve model: " ++ show problem)
        exitFailure
  putStr (renderModelWith environment model)

covering
emitLanguageMiniZinc : String -> IO ()
emitLanguageMiniZinc path = withLanguageModel path $ \model =>
  case renderMiniZinc model of
    Left problem => do
      putStrLn ("Cannot lower model to MiniZinc: " ++ problem)
      exitFailure
    Right source => putStr source

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
    [_, "check", path] => checkLanguage path
    [_, "build", path] => buildLanguage path
    [_, "minizinc", path] => emitLanguageMiniZinc path
    [_, "--solve", requested] => emitSolved requested
    [_, "--minizinc", requested] => emitMiniZinc requested
    [_, requested] => emit requested
    _ => do
      putStrLn usage
      exitFailure
