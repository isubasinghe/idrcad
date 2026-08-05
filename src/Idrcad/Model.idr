module Idrcad.Model

import Idrcad.Constraint
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry

%default total

public export
record Parameter where
  constructor MkParameter
  parameterName : String
  lowerBound : Fixed
  defaultValue : Fixed
  upperBound : Fixed

public export
data Objective
  = Satisfy
  | Maximize Expr
  | Minimize Expr

public export
record Model (dimension : Dimension) where
  constructor MkModel
  modelName : String
  modelParameters : List Parameter
  modelConstraints : List Constraint
  modelObjective : Objective
  modelGeometry : Shape dimension

public export
defaultEnvironment : Model dimension -> Environment
defaultEnvironment model = map toPair model.modelParameters
  where
    toPair : Parameter -> (String, Fixed)
    toPair parameter = (parameter.parameterName, parameter.defaultValue)

public export
defaultsAreValid : (equalityTolerance : Fixed) -> Model dimension -> Bool
defaultsAreValid tolerance model =
  validate tolerance (defaultEnvironment model) model.modelConstraints

lookupBinding : String -> Environment -> Maybe Fixed
lookupBinding name [] = Nothing
lookupBinding name ((candidate, value) :: rest) =
  if name == candidate then Just value else lookupBinding name rest

parameterIsValid : Environment -> Parameter -> Bool
parameterIsValid environment parameter =
  case lookupBinding parameter.parameterName environment of
    Just (MkFixed value) =>
      let MkFixed lower = parameter.lowerBound
          MkFixed upper = parameter.upperBound
       in lower <= value && value <= upper
    Nothing => False

parametersAreValid : Environment -> List Parameter -> Bool
parametersAreValid environment [] = True
parametersAreValid environment (parameter :: rest) =
  parameterIsValid environment parameter && parametersAreValid environment rest

||| Check a solver assignment before allowing it to enter generated geometry.
||| Solver output is untrusted: every parameter must be present and bounded,
||| and every symbolic constraint must hold exactly.
public export
solutionIsValid : Environment -> Model dimension -> Bool
solutionIsValid environment model =
  parametersAreValid environment model.modelParameters
    && validate (millionths 0) environment model.modelConstraints
