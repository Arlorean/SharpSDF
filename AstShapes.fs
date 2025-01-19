module SharpSDF.AstShapes

open Ast
open type Ast.Intrinsics


//
// ShapeFns can be composed. The shapes below take parameters from the F# world and return us a lifted AST shape
// which can be composed and then eventually evaluated
//
// Lower case floatN -> F# world
// Upper case FloatN -> AST world
// 
// Define lots of simple, reusable primitives like "circle" and "roundedbox"
// Compose them to make higher-level shapes like "snowman"

let empty : ShapeFn = 
    fun (_ : Float2) -> f1 (System.Double.MaxValue)

let circle (r: float) : ShapeFn =
    fun (p : Float2) ->
        length p - r

let snowman : ShapeFn =
    circle 40
    <+>
    (circle 60 |> translate 0 70)
    |> translate 0 -40

let roundedBox (hs:HLSL.float2) (tr:float,br:float,tl:float,bl:float) : ShapeFn =
    fun (p : Float2) ->        
        let tb = 
            if2_ (gt__ p.x 0)
                (f2__  tr br)
                (f2__  tl bl)
        
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
