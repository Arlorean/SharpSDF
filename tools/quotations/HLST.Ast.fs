module SharpSDF.HLSL_Ast

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
    | From of Expr
    | Int of int
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
    | Let of (string * Expr) list  * Float
    static member (-) (v1:Float, v2:Float) = Float.Sub(v1,v2)
    static member (+) (v1:Float, v2:Float) = Float.Add(v1,v2)
    static member inline op_GreaterThan (v1:Float, v2:Float) = Bool.Gt(v1, v2)
    static member inline op_GreaterThan (v1:Float, f : float) = Bool.Gt(v1, Float.Float f)
    static member Zero = Float.Float 0

and [<RequireQualifiedAccess>] Float2 =
    | Float2 of (Float * Float)
    | From of Expr
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
    | Let of (string * Expr) list  * Float2
    static member (-) (v1:Float2, v2:Float2) = Float2.Sub2(v1,v2)
    static member (+) (v1:Float2, v2:Float2) = Float2.Add2(v1,v2)
    member __.x = Float.X __
    member __.y = Float.Y __
    static member Zero = Float2.Float2 (Float.Zero, Float.Zero)

and [<RequireQualifiedAccess>] Expr =
    | F of Float
    | F2 of Float2
    | B of Bool

let f_ (value) = Float.Float value
let f2_ x y = Float2.Float2 (x,y)
let f2__ x y = Float2.Float2 (f_ x, f_ y)

let v_ (name) = Float.Var name
let let_ (defns) (expr) = Float.Let (defns, expr)
let if_ c t f = Float.If (c, t, f)
let if2_ c t f = Float2.If (c, t, f)
let let2_ (defns) (expr) = Float2.Let (defns, expr)

let gt_ a b = Bool.Gt( a, b )
let gt__ a b = Bool.Gt( a, f_ b )


type float1 =
    static member mk( v : float ) = Float.Float v
    
type float2 =
    static member mk (x:Float, y:Float) = Float2.Float2 (x,y)
    static member mk  (x:Float) = Float2.Float2(x,x)

    static member mk  (x:int, y:int) = Float2.Float2( Float.Int x, Float.Int y)

    static member (*) (v:Float2, t:Float) = Float2.Mul(v,t)

    static member (+) (v1:Float2, v2:Float2) = Float2.Add2(v1,v2)
    static member (-) (v1:Float2, v2:Float2) = Float2.Sub2(v1,v2)
    static member (*) (v1:Float2, v2:Float2) = Float2.Mul2(v1,v2)
    static member (/) (v1:Float2, v2:Float2) = Float2.Div2(v1,v2)
    
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
    
// Define an AST type

let compile (shaderAst : Float2 -> Float) : (float2 -> float) =
    Unchecked.defaultof<_>
    
