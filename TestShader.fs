module SharpSDF.TestShader

open Shaders
open Colors

let sdfShader =
    outerShadow gray50 30.0 |>>
    solidFill blue |>> 
    innerShadow gray50 30.0 |>> 
    solidStroke red 20.0

#if !!USE_AST2
open Ast
open AstShapes
open Compiler
#else
open Ast2
open AstShapes2
open Compiler2
let f1 (v:System.Double) = float(v)
#endif

let snowman =
    circle 40
    <+>
    (circle 60 |> translate 0 70)
    |> translate 0 -40

let smallerSnowman = 
    snowman |> scale 0.7

let snowScene =
    [1.0 .. 5.0]
    |> List.fold (fun shape i ->
        shape <+>
        (smallerSnowman 
            |> scale (f1 (1.0/ i))
            |> translate (f1 (i * -58.0)) 0
        )
    ) empty
    |> translate 160 0

let private shape = snowScene

let background = transparent

let shader position =
    position 
    |> (shape |> compileToInterpreter)
    |> sdfShader background
