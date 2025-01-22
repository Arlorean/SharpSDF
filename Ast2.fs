module SharpSDF.Ast2

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AbstractClass>]
type Expr(op:Op) =
    // Needed to overload op_GreaterThan operator for float, float2, etc.
    interface IComparable with 
        member this.CompareTo (that) = failwithf "(%A).CompareTo(%A)" this that
    member _.op = op // The generic Op for this typesafe wrapper
    member this.Dot([<CallerMemberName;Optional;DefaultParameterValue("")>] memberName:string) = 
        Dot(this, memberName)

and UnaryFn =
    | Abs
    | Exp
    | Plus
    | Minus
and BinaryFn =
    | Add
    | Sub
    | Mul
    | Div
    | Rem
    | Min
    | Max
    | Step
and TernaryFn =
    | Clamp
    | Lerp
    | Smoothstep
and ComparisonFn =
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE
and Op =
    // Literals
    | Float of System.Double
    | Bool of System.Boolean
    | Int of System.Int32
    | FloatN of System.Double[]
    // Binary Arithmetic
    | Unary of UnaryFn * Expr
    | Binary of BinaryFn * Expr * Expr
    | Ternary of TernaryFn * Expr * Expr * Expr
    | Comparison of ComparisonFn * Expr * Expr // Returns bool[N]
    // Special
    | IfThenElse of Expr * Expr * Expr
    | Vector of Expr[]
    | Length of Expr
    | Dot of Expr * string

type bool =
    inherit Expr
    new (v) = { inherit Expr(Bool(v)) }
    internal new (op:Op) = { inherit Expr(op) }
    static member Wrap(op:Op) = bool(op)
    static member op_Implicit(v:Boolean) = bool(v)

type int =
    inherit Expr
    new (v) = { inherit Expr(Int(v)) }
    internal new (op:Op) = { inherit Expr(op) }
    static member Wrap(op:Op) = int(op)
    static member op_Implicit(v:Int32) = int(v)

type float =
    inherit Expr
    new (v) = { inherit Expr(Float(v)) }
    internal new (v:Op) = { inherit Expr(v) }
    static member Wrap(op:Op) = float(op)
    static member op_Implicit(v:Double) = float(v)
    static member op_Implicit(v:Int32) = float(v)
    static member (~+) (v:float) = float(Unary(Plus,v))
    static member (~-) (v:float) = float(Unary(Minus,v))
    static member (+)  (v1:float, v2:float) = float(Binary(Add,v1,v2))
    static member (-)  (v1:float, v2:float) = float(Binary(Sub,v1,v2))
    static member op_GreaterThan (v1:float, v2:float) = bool(Comparison(GT,v1,v2))
    static member op_Inequality (v1:float, v2:float) = bool(Comparison(NE,v1,v2))
    static member op_Equality (v1:float, v2:float) = bool(Comparison(EQ,v1,v2))


type float2 =
    inherit Expr
    new (x:float,y:float) = { inherit Expr(Vector([|x;y|])) }
    new (v:float) = float2(v,v)
    new (t:float*float) = let (x,y) = t in float2(x,y)
    new (t:Double*Double) = let (x,y) = t in float2(x,y)
    internal new (op:Op) = { inherit Expr(op) }
    static member Wrap(op:Op) = float2(op)
    static member op_Implicit(v:Int32) = float2(float v)
    static member op_Implicit(v:Double) = float2(v)
    member this.x = float(this.Dot())
    member this.y = float(this.Dot())
    static member (+) (v1:float2, v2:float) = float2(Binary(Add,v1,float2(v2)))
    static member (-) (v1:float2, v2:float) = float2(Binary(Sub,v1,float2(v2)))
    static member (*) (v1:float2, v2:float) = float2(Binary(Mul,v1,float2(v2)))
    static member (/) (v1:float2, v2:float) = float2(Binary(Div,v1,float2(v2)))
    static member (+) (v1:float2, v2:float*float) = float2(Binary(Add,v1,float2(v2)))
    static member (+) (v1:float2, v2:float2) = float2(Binary(Add,v1,v2))
    static member (-) (v1:float2, v2:float2) = float2(Binary(Sub,v1,v2))
    static member (*) (v1:float2, v2:float2) = float2(Binary(Mul,v1,v2))
    static member (/) (v1:float2, v2:float2) = float2(Binary(Div,v1,v2))


type float3 = 
    inherit Expr
    new (v:float) = float3(v,v,v)
    new (x:float,y:float,z:float) = { inherit Expr(Vector([|x;y;z|])); }
    new (t:(Double*Double*Double)) = let (x,y,z) = t in float3(x,y,z)
    internal new (op:Op) = { inherit Expr(op) }
    static member Wrap(op:Op) = float3(op)
    member this.x = float(this.Dot())
    member this.y = float(this.Dot())
    member this.z = float(this.Dot())

type float4 = 
    inherit Expr
    new (v:float) = float4(v,v,v,v)
    new (x:float,y:float,z:float,w:float) = { inherit Expr(Vector([|x;y;z;w|])) }
    new (t:(Double*Double*Double*Double)) = let (x,y,z,w) = t in float4(x,y,z,w)
    internal new (op:Op) = { inherit Expr(op) }
    static member Wrap(op:Op) = float4(op)
    member this.x = float(this.Dot())
    member this.y = float(this.Dot())
    member this.z = float(this.Dot())
    member this.w = float(this.Dot())
    member this.xy = float3(this.Dot())
    member this.zw = float3(this.Dot())
    member this.xyz = float3(this.Dot())
    member this.r = float(this.Dot())
    member this.g = float(this.Dot())
    member this.b = float(this.Dot())
    member this.a = float(this.Dot())
    member this.rgb = float3(this.Dot())


// Create a typesafe wrapper for a generic Op e.g. float, float2, etc.
let inline Wrap<'T when 'T:(static member Wrap: Op->'T)> (op:Op) : 'T = 'T.Wrap(op)

// Build tuple
let inline (^) (t:'T) (f:'T) = (t,f)

// Syntax options for (if...then...else)
// If(condition)<| (trueValue,falseValue)
// If(condition)<| trueValue ^ falseValue
// (condition)?<| (trueValue,falseValue)
// (condition)?<| trueValue ^ falseValue
let inline If (cond:bool) (t:'T,f:'T) = Wrap<'T>(IfThenElse(cond,t,f))
let inline (?<|) (cond:bool) (t:'T,f:'T) = Wrap<'T>(IfThenElse(cond,t,f))


type Intrinsics =
    static member inline abs (v:'T):'T = Wrap(Unary(Abs,v))
    static member inline length (v:'T):float = Wrap(Length(v))
    static member inline max (v1:'T,v2:'T):'T = Wrap(Binary(Max,v1,v2))
    static member inline min (v1:'T,v2:'T):'T = Wrap(Binary(Min,v1,v2))

