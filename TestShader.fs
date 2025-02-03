module SharpSDF.TestShader

open Ast2
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
            |> scale (1.0/ i)
            |> translate (i * -58.0) 0
        )
    ) empty
    |> translate 160 0

//let private shape :ShapeFn = snowScene
let shape :ShapeFn = circle 40

let background = transparent

let shader position =
    position |> shape |> sdfShader background
