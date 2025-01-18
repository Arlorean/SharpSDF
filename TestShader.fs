module SharpSDF.TestShader

open Shaders
open Colors

let sdfShader =
    outerShadow gray50 30.0 |>>
    solidFill blue |>> 
    innerShadow gray50 30.0 |>> 
    solidStroke red 20.0
    
let shape0 = _circle 100.0 // Natively implemented shape

let shape1 = AstShapes.sdCircle_ 100.0

let shape2 = AstShapes.sdRoundedBox_ (HLSL.float2(100,100)) (20,20,20,20)

let shape = shape2
let background = clear

let shader position =
    position 
    |> Position.Create 
    |> (shape |> sdf) 
    |> sdfShader background
