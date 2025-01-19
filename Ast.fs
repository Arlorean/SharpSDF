module SharpSDF.Ast

[<RequireQualifiedAccess>]
type Bool =
    | Bool of bool

    | Eq of Float * Float
    | Ne of Float * Float
    | Lt of Float * Float
    | Le of Float * Float
    | Gt of Float * Float
    | Ge of Float * Float

    | Eq2 of Float2 * Float2
    
    | Not of Bool
    
and [<RequireQualifiedAccess>] Float =
    | Var of string
    | Float of float
    | Int of int
    | Negate of (Float)
    | Mul of (Float * Float)
    | Add of (Float * Float)
    | Sub of (Float * Float)
    | Div of (Float * Float)
    | Length of Float2
    | Abs of Float
    | Min of Float * Float
    | Max of Float * Float
    | X of Float2
    | Y of Float2
    | If of (Bool * Float * Float)
    static member (-) (v1:Float, v2:Float) = Float.Sub(v1,v2)
    static member (-) (v1:Float, v2:float) = Float.Sub(v1, Float.Float v2)
    static member (+) (v1:Float, v2:Float) = Float.Add(v1,v2)
    static member inline op_GreaterThan (v1:Float, v2:Float) = Bool.Gt(v1, v2)
    static member inline op_GreaterThan (v1:Float, f : float) = Bool.Gt(v1, Float.Float f)
    static member Of (v : float) = Float.Float v
    static member Zero = Float.Of 0

and [<RequireQualifiedAccess>] Float2 =
    | Float2 of (Float * Float)
    | Mul of (Float2 * Float)
    | Add of (Float2 * Float)
    | Sub of (Float2 * Float)
    | Mul2 of (Float2 * Float2)
    | Add2 of (Float2 * Float2)
    | Sub2 of (Float2 * Float2)
    | Div2 of (Float2 * Float2)
    | Min of Float2 * Float2
    | Max of Float2 * Float2
    | If of (Bool * Float2 * Float2)
    static member (-) (v1:Float2, v2:Float2) = Float2.Sub2(v1,v2)
    static member (-) (v1:Float2, v2:HLSL.float2) = Float2.Sub2(v1, Float2.Of v2)
    static member (+) (v1:Float2, v2:Float2) = Float2.Add2(v1,v2)
    member __.x = Float.X __
    member __.y = Float.Y __
    static member Zero = Float2.Float2 (Float.Zero, Float.Zero)
    static member Of (v : HLSL.float2) = Float2.Float2( Float.Of v.x, Float.Of v.y )

let f1 (value) = Float.Float value
let f2 (value : HLSL.float2) = Float2.Float2 (f1 value.x, f1 value.y)
let f2_ x y = Float2.Float2 (x,y)
let f2__ x y = Float2.Float2 (f1 x, f1 y)

let v_ (name) = Float.Var name
let if_ c t f = Float.If (c, t, f)
let if2_ c t f = Float2.If (c, t, f)

let gt_ a b = Bool.Gt( a, b )
let gt__ a b = Bool.Gt( a, f1 b )

type private F1 =
    static member internal Length(v:Float) = v
    static member internal Abs(v:Float) = Float.Abs v
    static member internal Min(a:Float,b:Float) = Float.Min(a,b)
    static member internal Max(a:Float,b:Float) = Float.Max(a,b)
    
type private F2 =
    static member internal Length(v:Float2) = Float.Length v
    static member internal Min(a:Float2,b:Float2) = Float2.Min(a,b)
    static member internal Max(a:Float2,b:Float2) = Float2.Max(a,b)
        
type Intrinsics =
    static member abs (v) = F1.Abs(v)
    static member min (a,b) = F1.Min(a,b) 
    static member min (a,b) = F2.Min(a,b) 
    static member max (a,b) = F1.Max(a,b) 
    static member max (a,b) = F2.Max(a,b) 
    static member length (v) = F1.Length(v)
    static member length (v) = F2.Length(v)
    
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

let translate x y (shape : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        Float2.Add2( p, f2__ x y ) |> shape

let scale (s : float) (shape : ShapeFn) : ShapeFn = 
    fun (p : Float2) ->
        Float2.Mul( p, f1 (1.0 / s) ) |> shape