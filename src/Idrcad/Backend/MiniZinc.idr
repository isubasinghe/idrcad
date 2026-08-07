module Idrcad.Backend.MiniZinc

import Idrcad.Constraint
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model

joinWith : String -> List String -> String
joinWith separator [] = ""
joinWith separator [value] = value
joinWith separator (value :: rest) = value ++ separator ++ joinWith separator rest

renderRelation : Relation -> String
renderRelation Equal = "="
renderRelation LessThan = "<"
renderRelation LessOrEqual = "<="
renderRelation GreaterThan = ">"
renderRelation GreaterOrEqual = ">="

||| Lower an exact fixed-point expression into raw integer ticks. This accepts
||| precisely the affine fragment reported by isIntegerLinear.
renderLinearExpr : Expr -> Maybe String
renderLinearExpr (Lit (MkFixed value)) = Just (show value)
renderLinearExpr (Var name) = Just name
renderLinearExpr (Add left right) =
  case (renderLinearExpr left, renderLinearExpr right) of
    (Just x, Just y) => Just ("(" ++ x ++ " + " ++ y ++ ")")
    _ => Nothing
renderLinearExpr (Subtract left right) =
  case (renderLinearExpr left, renderLinearExpr right) of
    (Just x, Just y) => Just ("(" ++ x ++ " - " ++ y ++ ")")
    _ => Nothing
renderLinearExpr (Multiply (Lit (MkFixed coefficient)) expression) =
  if coefficient `mod` fixedScale == 0
    then map
      (\rendered =>
        "(" ++ show (coefficient `div` fixedScale) ++ " * " ++ rendered ++ ")")
      (renderLinearExpr expression)
    else Nothing
renderLinearExpr (Multiply expression (Lit (MkFixed coefficient))) =
  if coefficient `mod` fixedScale == 0
    then map
      (\rendered =>
        "(" ++ rendered ++ " * " ++ show (coefficient `div` fixedScale) ++ ")")
      (renderLinearExpr expression)
    else Nothing
renderLinearExpr (Multiply left right) = Nothing
renderLinearExpr (Divide left right) = Nothing
renderLinearExpr (Negate expression) =
  map (\rendered => "(-" ++ rendered ++ ")") (renderLinearExpr expression)
renderLinearExpr (Modulo left right) = Nothing
renderLinearExpr (Power base exponent) = Nothing
renderLinearExpr (Sine angle) = Nothing
renderLinearExpr (Cosine angle) = Nothing
renderLinearExpr (ArcCosine value) = Nothing
renderLinearExpr (ArcTangent2 y x) = Nothing
renderLinearExpr (SquareRoot value) = Nothing
renderLinearExpr (Floor value) = Nothing

renderLinearExprs : List Expr -> Maybe (List String)
renderLinearExprs [] = Just []
renderLinearExprs (expression :: rest) =
  case (renderLinearExpr expression, renderLinearExprs rest) of
    (Just rendered, Just renderedRest) => Just (rendered :: renderedRest)
    _ => Nothing

footprintXs : List Footprint2D -> List Expr
footprintXs = map footprintX

footprintYs : List Footprint2D -> List Expr
footprintYs = map footprintY

footprintWidths : List Footprint2D -> List Expr
footprintWidths = map footprintWidth

footprintDepths : List Footprint2D -> List Expr
footprintDepths = map footprintDepth

renderArray : List String -> String
renderArray values = "[" ++ joinWith ", " values ++ "]"

renderPositiveDimensions : List String -> String
renderPositiveDimensions [] = ""
renderPositiveDimensions (dimension :: rest) =
  "constraint " ++ dimension ++ " > 0;\n"
    ++ renderPositiveDimensions rest

renderDiffn :
  List Footprint2D ->
  List String ->
  List String ->
  List String ->
  List String ->
  String
renderDiffn [] xs ys widths depths = ""
renderDiffn [footprint] xs ys widths depths = ""
renderDiffn footprints xs ys widths depths =
  "constraint diffn(" ++ renderArray xs ++ ", "
    ++ renderArray ys ++ ", " ++ renderArray widths ++ ", "
    ++ renderArray depths ++ ");"

renderConstraint : Constraint -> Either String String
renderConstraint (Constrain left relation right message) =
  case (renderLinearExpr left, renderLinearExpr right) of
    (Just x, Just y) => Right $
      "% " ++ message ++ "\nconstraint " ++ x ++ " "
        ++ renderRelation relation ++ " " ++ y ++ ";"
    _ => Left ("Constraint is not integer-linear: " ++ message)
renderConstraint (NonOverlapping footprints message) =
  case (renderLinearExprs (footprintXs footprints),
        renderLinearExprs (footprintYs footprints),
        renderLinearExprs (footprintWidths footprints),
        renderLinearExprs (footprintDepths footprints)) of
    (Just xs, Just ys, Just widths, Just depths) => Right $
      "% " ++ message ++ "\n"
        ++ renderPositiveDimensions widths
        ++ renderPositiveDimensions depths
        ++ renderDiffn footprints xs ys widths depths
    _ => Left ("Non-overlap constraint is not integer-linear: " ++ message)

renderConstraints : List Constraint -> Either String (List String)
renderConstraints [] = Right []
renderConstraints (constraint :: rest) =
  case (renderConstraint constraint, renderConstraints rest) of
    (Right rendered, Right renderedRest) => Right (rendered :: renderedRest)
    (Left error, _) => Left error
    (_, Left error) => Left error

validParameter : Parameter -> Bool
validParameter (MkParameter name (MkFixed lower) (MkFixed value) (MkFixed upper)) =
  lower <= value && value <= upper

renderParameter : Parameter -> Either String String
renderParameter parameter@(MkParameter name (MkFixed lower) value (MkFixed upper)) =
  if validParameter parameter
    then Right $
      "% default " ++ renderFixed value ++ "\nvar " ++ show lower ++ ".."
        ++ show upper ++ ": " ++ name ++ ";"
    else Left ("Invalid bounds/default for parameter: " ++ name)

renderParameters : List Parameter -> Either String (List String)
renderParameters [] = Right []
renderParameters (parameter :: rest) =
  case (renderParameter parameter, renderParameters rest) of
    (Right rendered, Right renderedRest) => Right (rendered :: renderedRest)
    (Left error, _) => Left error
    (_, Left error) => Left error

renderOutput : List Parameter -> String
renderOutput modelParams =
  let pieces = map
        (\parameter =>
          "\"" ++ parameter.parameterName ++ "=\", show("
            ++ parameter.parameterName ++ "), \"\\n\"")
        modelParams
   in "output [" ++ joinWith ", " pieces ++ "];"

parameterArray : List Parameter -> String
parameterArray modelParams =
  "[" ++ joinWith ", " (map parameterName modelParams) ++ "]"

renderObjective : List Parameter -> Objective -> Either String String
renderObjective modelParams Satisfy = Right "solve satisfy;"
renderObjective modelParams (Maximize expression) =
  case renderLinearExpr expression of
    Just rendered => Right $
      "solve :: int_search(" ++ parameterArray modelParams
        ++ ", first_fail, indomain_max, complete) maximize " ++ rendered ++ ";"
    Nothing => Left "Maximization objective is not integer-linear"
renderObjective modelParams (Minimize expression) =
  case renderLinearExpr expression of
    Just rendered => Right $
      "solve :: int_search(" ++ parameterArray modelParams
        ++ ", first_fail, indomain_min, complete) minimize " ++ rendered ++ ";"
    Nothing => Left "Minimization objective is not integer-linear"

||| Generate a solver-independent MiniZinc model over raw integer ticks.
||| It can be sent to CP-SAT because floats, division, and nonlinear products
||| are rejected during lowering.
public export
renderMiniZinc : Model dimension -> Either String String
renderMiniZinc model =
  case (renderParameters model.modelParameters,
        renderConstraints model.modelConstraints,
        renderObjective model.modelParameters model.modelObjective) of
    (Right parameterLines, Right constraintLines, Right objectiveLine) => Right $
      "% Generated by idrcad: " ++ model.modelName ++ "\n"
        ++ "% One whole CAD unit = " ++ show fixedScale ++ " integer ticks.\n\n"
        ++ "include \"globals.mzn\";\n\n"
        ++ joinWith "\n\n" parameterLines ++ "\n\n"
        ++ joinWith "\n\n" constraintLines ++ "\n\n"
        ++ objectiveLine ++ "\n\n"
        ++ renderOutput model.modelParameters ++ "\n"
    (Left error, _, _) => Left error
    (_, Left error, _) => Left error
    (_, _, Left error) => Left error
