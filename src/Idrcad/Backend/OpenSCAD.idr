module Idrcad.Backend.OpenSCAD

import Idrcad.Constraint
import Idrcad.Expr
import Idrcad.Fixed
import Idrcad.Geometry
import Idrcad.Model

joinWith : String -> List String -> String
joinWith separator [] = ""
joinWith separator [value] = value
joinWith separator (value :: rest) = value ++ separator ++ joinWith separator rest

indent : Nat -> String
indent Z = ""
indent (S depth) = "  " ++ indent depth

scadBool : Bool -> String
scadBool True = "true"
scadBool False = "false"

escapeChar : Char -> String
escapeChar '"' = "\\\""
escapeChar '\\' = "\\\\"
escapeChar '\n' = "\\n"
escapeChar '\r' = "\\r"
escapeChar '\t' = "\\t"
escapeChar character = pack [character]

escapeString : String -> String
escapeString value = concat (map escapeChar (unpack value))

quoted : String -> String
quoted value = "\"" ++ escapeString value ++ "\""

public export
renderExpr : Expr -> String
renderExpr (Lit value) = renderFixed value
renderExpr (Var name) = name
renderExpr (Add left right) =
  "(" ++ renderExpr left ++ " + " ++ renderExpr right ++ ")"
renderExpr (Subtract left right) =
  "(" ++ renderExpr left ++ " - " ++ renderExpr right ++ ")"
renderExpr (Multiply left right) =
  "(" ++ renderExpr left ++ " * " ++ renderExpr right ++ ")"
renderExpr (Divide left right) =
  "(" ++ renderExpr left ++ " / " ++ renderExpr right ++ ")"
renderExpr (Negate expression) = "(-" ++ renderExpr expression ++ ")"
renderExpr (Modulo left right) =
  "(" ++ renderExpr left ++ " % " ++ renderExpr right ++ ")"
renderExpr (Power base exponent) =
  "pow(" ++ renderExpr base ++ ", " ++ renderExpr exponent ++ ")"
renderExpr (Sine angle) = "sin(" ++ renderExpr angle ++ ")"
renderExpr (Cosine angle) = "cos(" ++ renderExpr angle ++ ")"
renderExpr (ArcCosine value) = "acos(" ++ renderExpr value ++ ")"
renderExpr (ArcTangent2 y x) =
  "atan2(" ++ renderExpr y ++ ", " ++ renderExpr x ++ ")"
renderExpr (SquareRoot value) = "sqrt(" ++ renderExpr value ++ ")"
renderExpr (Floor value) = "floor(" ++ renderExpr value ++ ")"

renderVec2 : Vec2 -> String
renderVec2 (MkVec2 x y) = "[" ++ renderExpr x ++ ", " ++ renderExpr y ++ "]"

renderVec3 : Vec3 -> String
renderVec3 (MkVec3 x y z) =
  "[" ++ renderExpr x ++ ", " ++ renderExpr y ++ ", " ++ renderExpr z ++ "]"

renderArgs : List (String, String) -> String
renderArgs arguments =
  joinWith ", " (map (\(name, value) => name ++ " = " ++ value) arguments)

call : String -> List (String, String) -> String
call name arguments = name ++ "(" ++ renderArgs arguments ++ ")"

maybeArg : String -> (value -> String) -> Maybe value -> List (String, String)
maybeArg name render Nothing = []
maybeArg name render (Just value) = [(name, render value)]

renderHorizontal : HorizontalAlignment -> String
renderHorizontal AlignLeft = quoted "left"
renderHorizontal AlignCenter = quoted "center"
renderHorizontal AlignRight = quoted "right"

renderVertical : VerticalAlignment -> String
renderVertical AlignTop = quoted "top"
renderVertical AlignCenterV = quoted "center"
renderVertical AlignBaseline = quoted "baseline"
renderVertical AlignBottom = quoted "bottom"

renderColour : Colour -> String
renderColour (NamedColour name) = quoted name
renderColour (RGB red green blue) =
  "[" ++ joinWith ", " (map renderExpr [red, green, blue]) ++ "]"
renderColour (RGBA red green blue alpha) =
  "[" ++ joinWith ", " (map renderExpr [red, green, blue, alpha]) ++ "]"

renderExtrudeScale : ExtrudeScale -> String
renderExtrudeScale (UniformExtrude factor) = renderExpr factor
renderExtrudeScale (XYExtrude vector) = renderVec2 vector

renderLinearOptions : LinearExtrudeOptions -> List (String, String)
renderLinearOptions (MkLinearExtrudeOptions height center twist scale convexity) =
  [("height", renderExpr height), ("center", scadBool center)]
    ++ maybeArg "twist" renderExpr twist
    ++ maybeArg "scale" renderExtrudeScale scale
    ++ maybeArg "convexity" show convexity

renderRotateOptions : RotateExtrudeOptions -> List (String, String)
renderRotateOptions (MkRotateExtrudeOptions angle convexity) =
  maybeArg "angle" renderExpr angle ++ maybeArg "convexity" show convexity

renderResolutionArgs : Resolution -> List (String, String)
renderResolutionArgs (MkResolution fragments angle size) =
  maybeArg "$fn" show fragments
    ++ maybeArg "$fa" renderExpr angle
    ++ maybeArg "$fs" renderExpr size

renderRelation : Relation -> String
renderRelation Equal = "=="
renderRelation LessThan = "<"
renderRelation LessOrEqual = "<="
renderRelation GreaterThan = ">"
renderRelation GreaterOrEqual = ">="

renderConstraint : Constraint -> String
renderConstraint (Constrain left relation right message) =
  "assert(" ++ renderExpr left ++ " " ++ renderRelation relation ++ " "
    ++ renderExpr right ++ ", " ++ quoted message ++ ");"

renderBlock : Nat -> String -> String -> String
renderBlock depth header body =
  indent depth ++ header ++ " {\n"
    ++ body ++ "\n"
    ++ indent depth ++ "}"

||| Render a dimension-checked geometry tree as OpenSCAD source.
public export
renderShape : Nat -> Shape dimension -> String
renderShape depth (Square size centered) =
  indent depth ++ call "square"
    [("size", renderVec2 size), ("center", scadBool centered)] ++ ";"
renderShape depth (Circle radius) =
  indent depth ++ call "circle" [("r", renderExpr radius)] ++ ";"
renderShape depth (Polygon points) =
  indent depth ++ call "polygon"
    [("points", "[" ++ joinWith ", " (map renderVec2 points) ++ "]")] ++ ";"
renderShape depth (Text2D (MkTextOptions content size font horizontal vertical spacing)) =
  indent depth ++ call "text"
    ([("text", quoted content),
      ("size", renderExpr size),
      ("halign", renderHorizontal horizontal),
      ("valign", renderVertical vertical),
      ("spacing", renderExpr spacing)]
      ++ maybeArg "font" quoted font) ++ ";"
renderShape depth (Import2D path layer) =
  indent depth ++ call "import"
    ([("file", quoted path)] ++ maybeArg "layer" quoted layer) ++ ";"
renderShape depth (Cube size centered) =
  indent depth ++ call "cube"
    [("size", renderVec3 size), ("center", scadBool centered)] ++ ";"
renderShape depth (Sphere radius) =
  indent depth ++ call "sphere" [("r", renderExpr radius)] ++ ";"
renderShape depth (Cylinder height bottomRadius topRadius centered) =
  indent depth ++ call "cylinder"
    [("h", renderExpr height),
     ("r1", renderExpr bottomRadius),
     ("r2", renderExpr topRadius),
     ("center", scadBool centered)] ++ ";"
renderShape depth (Polyhedron points faces) =
  indent depth ++ call "polyhedron"
    [("points", "[" ++ joinWith ", " (map renderVec3 points) ++ "]"),
     ("faces", "[" ++ joinWith ", " (map renderFace faces) ++ "]")] ++ ";"
  where
    renderFace : List Nat -> String
    renderFace indices = "[" ++ joinWith ", " (map show indices) ++ "]"
renderShape depth (Import3D path) =
  indent depth ++ call "import" [("file", quoted path)] ++ ";"
renderShape depth (Surface path centered) =
  indent depth ++ call "surface"
    [("file", quoted path), ("center", scadBool centered)] ++ ";"
renderShape depth (Union children) =
  renderBlock depth "union()"
    (joinWith "\n" (map (renderShape (S depth)) children))
renderShape depth (Difference positive negatives) =
  renderBlock depth "difference()"
    (joinWith "\n" (map (renderShape (S depth)) (positive :: negatives)))
renderShape depth (Intersection children) =
  renderBlock depth "intersection()"
    (joinWith "\n" (map (renderShape (S depth)) children))
renderShape depth (Hull children) =
  renderBlock depth "hull()"
    (joinWith "\n" (map (renderShape (S depth)) children))
renderShape depth (Translate2D vector child) =
  renderBlock depth (call "translate" [("v", renderVec2 vector)])
    (renderShape (S depth) child)
renderShape depth (Translate3D vector child) =
  renderBlock depth (call "translate" [("v", renderVec3 vector)])
    (renderShape (S depth) child)
renderShape depth (Rotate2D angle child) =
  renderBlock depth (call "rotate" [("a", renderExpr angle)])
    (renderShape (S depth) child)
renderShape depth (Rotate3D angles child) =
  renderBlock depth (call "rotate" [("a", renderVec3 angles)])
    (renderShape (S depth) child)
renderShape depth (Scale2D factors child) =
  renderBlock depth (call "scale" [("v", renderVec2 factors)])
    (renderShape (S depth) child)
renderShape depth (Scale3D factors child) =
  renderBlock depth (call "scale" [("v", renderVec3 factors)])
    (renderShape (S depth) child)
renderShape depth (Colourise colour child) =
  renderBlock depth (call "color" [("c", renderColour colour)])
    (renderShape (S depth) child)
renderShape depth (Highlight child) =
  renderBlock depth "#union()" (renderShape (S depth) child)
renderShape depth (Background child) =
  renderBlock depth "%union()" (renderShape (S depth) child)
renderShape depth (WithResolution resolution child) =
  case renderResolutionArgs resolution of
    [] => renderShape depth child
    arguments => renderBlock depth (call "let" arguments)
      (renderShape (S depth) child)
renderShape depth (LinearExtrude options child) =
  renderBlock depth (call "linear_extrude" (renderLinearOptions options))
    (renderShape (S depth) child)
renderShape depth (RotateExtrude options child) =
  renderBlock depth (call "rotate_extrude" (renderRotateOptions options))
    (renderShape (S depth) child)
renderShape depth (Projection cut child) =
  renderBlock depth (call "projection" [("cut", scadBool cut)])
    (renderShape (S depth) child)
renderShape depth (Roof method child) =
  let methodName = case method of
        StraightSkeleton => "straight"
        Voronoi => "voronoi"
   in renderBlock depth (call "roof" [("method", quoted methodName)])
        (renderShape (S depth) child)
renderShape depth (Offset2D mode child) =
  let arguments = case mode of
        RadialOffset radius => [("r", renderExpr radius)]
        DeltaOffset delta chamfer =>
          [("delta", renderExpr delta), ("chamfer", scadBool chamfer)]
   in renderBlock depth (call "offset" arguments)
        (renderShape (S depth) child)

renderParameter : Parameter -> String
renderParameter (MkParameter name lower value upper) =
  name ++ " = " ++ renderFixed value ++ ";"

lookupBinding : String -> Environment -> Maybe Fixed
lookupBinding name [] = Nothing
lookupBinding name ((candidate, value) :: rest) =
  if name == candidate then Just value else lookupBinding name rest

renderParameterWith : Environment -> Parameter -> String
renderParameterWith environment parameter =
  let value = case lookupBinding parameter.parameterName environment of
        Just solved => solved
        Nothing => parameter.defaultValue
   in parameter.parameterName ++ " = " ++ renderFixed value ++ ";"

||| Render parameters, executable assertions, and geometry into one document.
public export
renderModel : Model dimension -> String
renderModel model =
  let parameterLines = map renderParameter model.modelParameters
      constraintLines = map renderConstraint model.modelConstraints
      preludeLines = parameterLines ++ constraintLines
      prelude = case preludeLines of
        [] => ""
        lines => joinWith "\n" lines ++ "\n\n"
   in "// Generated by idrcad: " ++ model.modelName ++ "\n\n"
        ++ prelude ++ renderShape 0 model.modelGeometry ++ "\n"

||| Render a model using a checked solver environment rather than defaults.
||| Callers should first establish `solutionIsValid environment model`.
public export
renderModelWith : Environment -> Model dimension -> String
renderModelWith environment model =
  let parameterLines = map (renderParameterWith environment) model.modelParameters
      constraintLines = map renderConstraint model.modelConstraints
      preludeLines = parameterLines ++ constraintLines
      prelude = case preludeLines of
        [] => ""
        lines => joinWith "\n" lines ++ "\n\n"
   in "// Generated by idrcad after solving: " ++ model.modelName ++ "\n\n"
        ++ prelude ++ renderShape 0 model.modelGeometry ++ "\n"
