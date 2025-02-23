module SharpSDF.TestShader

open Wrappers
open AstShapes2

open Shaders
open Colors

let sdfShader :Shader =
    outerShadow gray50 30.0 |>>
    solidFill blue |>> 
    innerShadow gray50 30.0 |>> 
    solidStroke red 20.0

let snowman =
    circle 40
    <+>
    (circle 60 |> translate 0 70)
    |> translate 0 -40

let smallerSnowman = 
    snowman |> scale 0.7

let snowScene : ShapeFn =
    [1.0 .. 5.0]
    |> List.fold (fun shape i ->
        shape <+>
        (smallerSnowman 
            |> scale (1.0 / i)
            |> translate (i * -58.0) 0
        )
    ) empty
    |> translate 160 0

let private shape :ShapeFn = snowScene

let private position = float2(float(Ast.Float.Varying "p.x"), float(Ast.Float.Varying "p.y"))


let background = transparent

let shader =
    position |> shape |> sdfShader background

let shader2 =
    position |> circle 40 |> sdfShader background

let shader3 =
    red
