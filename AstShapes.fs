module SharpSDF.AstShapes

open Ast
open type Ast.Intrinsics

let sdCircle (r: Float) (p : Float2) =
    length p - r


let sdRoundedBox (hs:Float2) (tr:Float,br:Float,tl:Float,bl:Float) (p:Float2) : Float =
    
    let tb = 
        if2_ (gt__ p.x 0)
            (f2_  tr br)
            (f2_  br tr)
    
    let t = tb.x
    let b = tb.y

    let r = if_ (gt__ p.y 0) t b

    let q = 
        (f2_ (abs p.x) (abs p.y))
        - hs
        + (f2_ r r)

    let a =
        min (max (q.x,q.y), f1 0)

    let b = 
        max (q, Float2.Zero) |> length 

    (a + b - r)

// Create an SdfShape from an AST shape function. SdfShapes can be passed to the "sdf" function
let make (name : string) (shape : Ast.ShapeFn) : Shaders.SdfShape = 
    shape |> Compiler.compileToInterpreter |> Shaders.mkShapeBasic name

let sdRoundedBox_ (hs : HLSL.float2) (tr:float,br:float,tl:float,bl:float) =
    sdRoundedBox (f2 hs) (f1 tr, f1 br, f1 tl, f1 bl) 
    |> make (sprintf "rounded-box %f,%f" hs.x hs.y)

let sdCircle_ (r : float) = 
    sdCircle (f1 r) 
    |> make (sprintf "circle %f" r)

