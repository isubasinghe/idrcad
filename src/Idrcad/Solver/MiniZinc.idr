module Idrcad.Solver.MiniZinc

import Data.String
import Idrcad.Backend.MiniZinc
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model
import System.File

%default total

public export
data SolveFailure
  = LoweringFailed String
  | CouldNotStart
  | CouldNotWrite
  | CouldNotRead
  | SolverExited Int String
  | InvalidSolution String

public export
Show SolveFailure where
  show (LoweringFailed problem) = problem
  show CouldNotStart =
    "Could not start MiniZinc. Is `minizinc` available on PATH?"
  show CouldNotWrite = "Could not send the generated model to MiniZinc"
  show CouldNotRead = "Could not read MiniZinc's solution"
  show (SolverExited code output) =
    "MiniZinc exited with code " ++ show code ++ ":\n" ++ output
  show (InvalidSolution problem) = problem

parseBinding : String -> Maybe (String, Fixed)
parseBinding source =
  let (name, assignment) = break (== '=') (trim source)
   in case unpack assignment of
        '=' :: valueCharacters =>
          case parseInteger {a = Integer} (pack valueCharacters) of
            Just value => Just (trim name, MkFixed value)
            Nothing => Nothing
        _ => Nothing

parseBindings : List String -> Environment
parseBindings [] = []
parseBindings (line :: rest) =
  case parseBinding line of
    Just binding => binding :: parseBindings rest
    Nothing => parseBindings rest

||| Invoke MiniZinc over a bidirectional pipe. The generated model is passed on
||| stdin, so solving does not leave temporary files behind.
covering
invokeMiniZinc : String -> String -> IO (Either SolveFailure String)
invokeMiniZinc solver source = do
  Right process <- System.File.Process.Escaped.popen2
    [ "minizinc"
    , "--solver", solver
    , "--no-output-comments"
    , "-"
    ]
    | Left error => pure (Left CouldNotStart)
  writeResult <- fPutStr process.input source
  closeFile process.input
  case writeResult of
    Left error => do
      closeFile process.output
      _ <- popen2Wait process
      pure (Left CouldNotWrite)
    Right () => do
      readResult <- fRead process.output
      closeFile process.output
      exitCode <- popen2Wait process
      case readResult of
        Left error => pure (Left CouldNotRead)
        Right output =>
          if exitCode == 0
            then pure (Right output)
            else pure (Left (SolverExited exitCode output))

||| Solve with MiniZinc, then independently check every returned binding and
||| constraint in Idris before accepting the environment.
public export
covering
solveWith : (solver : String) -> Model dimension -> IO (Either SolveFailure Environment)
solveWith solver model =
  case renderMiniZinc model of
    Left problem => pure (Left (LoweringFailed problem))
    Right source => do
      Right output <- invokeMiniZinc solver source
        | Left problem => pure (Left problem)
      -- Optimization may print improving solutions. Reading bottom-to-top
      -- makes lookup select bindings from the final (optimal) solution.
      let environment = parseBindings (reverse (lines output))
      if solutionIsValid environment model
        then pure (Right environment)
        else pure (Left (InvalidSolution
          ("MiniZinc returned an incomplete or invalid solution:\n" ++ output)))

||| Use the CP solver bundled with the project's MiniZinc package.
public export
covering
solve : Model dimension -> IO (Either SolveFailure Environment)
solve = solveWith "gecode"
