module SharpSDF.AstShapes2

open Ast2
open type Ast2.Intrinsics

type ShapeFn = float2 -> float

// Summary:
// 	•	Union: min(sdf1, sdf2)
// 	•	Intersection: max(sdf1, sdf2)
// 	•	Difference: max(sdf1, -sdf2)

let union (a:ShapeFn) (b:ShapeFn) :ShapeFn=fun(p)-> 
    min( p |> a, p |> b ) 

let intersection (a:ShapeFn) (b:ShapeFn) :ShapeFn=fun(p)-> 
    max( p |> a, p |> b ) 

let difference (a:ShapeFn) (b:ShapeFn) :ShapeFn=fun(p)-> 
    max( p |> a, -(p |> b)) 

let (<->) = difference    // Difference just can't be separated from the notion of "subtraction"
let (<&>) = intersection  // Bits of the shape in A AND B
// let (<|>) = union         // Bits of the shape in A OR B. Complements intersection / (&) nicely but arguably not as easy to think about as (+)
let (<+>) = union         // Adding the shapes.

let translate (x:float) (y:float) (shape:ShapeFn) :ShapeFn=fun(p)-> 
    (p + (x,y)) |> shape

let scale (s:float) (shape:ShapeFn) :ShapeFn=fun(p)-> 
    (p / s) |> shape

//
// ShapeFns can be composed. The shapes below take parameters from the F# world and return us a lifted AST shape
// which can be composed and then eventually evaluated
//

let empty :ShapeFn=fun(_)->
    System.Double.MaxValue

let circle (r:float) :ShapeFn=fun(p)->
    length(p) - r

// Corner Radius (r:float4) = float4(tr,br,tl,bl)
let roundedBox (hs:float2) (r:float4) :ShapeFn=fun(p)->
    let r = (p.x>0)?<| r.xy ^ r.zw
    let r = (p.y>0)?<| r.x  ^ r.y
    let q = abs(p) - hs + r
    min(max(q.x, q.y), 0) + length(max(q, 0)) - r

