module SharpSDF.TestShader

open Shaders
open Colors

open Ast

let sdfShader =
    outerShadow gray50 30.0 |>>
    solidFill blue |>> 
    innerShadow gray50 30.0 |>> 
    solidStroke red 20.0
    
let shape0 = SDF.sdCircle 100.0 // Natively implemented shape

let shape1 = AstShapes.circle 100.0

let shape2 = AstShapes.roundedBox (HLSL.float2(100,100)) (20,20,20,20)


let snowman : ShapeFn =
    AstShapes.circle 40
    <+>
    (AstShapes.circle 60 |> translate 0 70)
    |> translate 0 -40

let smallerSnowman = 
    AstShapes.snowman |> Ast.scale 0.7

let snowScene =
    [1.0..3.0]
    |> List.fold (fun shape i ->
        shape <+>
        (smallerSnowman 
            |> scale (1.0 / i)
            |> translate (i * -80.0) 0
        )

    ) AstShapes.empty
    |> translate 150 0


let private shape = snowScene
let private background = clear

let shader position =
    position 
    |> (shape |> Compiler.compileToInterpreter)
    |> sdfShader background
