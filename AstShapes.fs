module SharpSDF.AstShapes

open Ast
open type Ast.Intrinsics

type ShapeFn = Float2 -> Float

// Summary:
// 	•	Union: min(sdf1, sdf2)
// 	•	Intersection: max(sdf1, sdf2)
// 	•	Difference: max(sdf1, -sdf2)

let union (a : ShapeFn) (b : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        Float.Min( p |> a, p |> b ) 

let intersection (a : ShapeFn) (b : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        Float.Max( p |> a, p |> b ) 

let difference (a : ShapeFn) (b : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        Float.Max( p |> a, p |> b |> Float.Negate ) 

let (<->) = difference    // Difference just can't be separated from the notion of "subtraction"
let (<&>) = intersection  // Bits of the shape in A AND B
// let (<|>) = union         // Bits of the shape in A OR B. Complements intersection / (&) nicely but arguably not as easy to think about as (+)
let (<+>) = union         // Adding the shapes.

let translate (x:Float) (y:Float) (shape : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        (Float2.Add2( p, Float2.Float2(x,y) )) |> shape

let scale (s : Float) (shape : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        (Float2.Mul( p, Float.Float(1.0) / s )) |> shape

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

let circle (r: Float) : ShapeFn =
    fun (p : Float2) ->
        length p - r

let roundedBox (hs:Float2) (tr:Float,br:Float,tl:Float,bl:Float) : ShapeFn =
    fun (p : Float2) ->        
        let tb = 
            if2_ (gt__ p.x 0)
                (f2_ tr br)
                (f2_ tl bl)
        
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


