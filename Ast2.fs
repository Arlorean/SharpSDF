module SharpSDF.Ast2

#nowarn 86
#nowarn 3535

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AbstractClass>]
type Expr(op:Op) =
    member _.op = op // The generic Op for this typesafe wrapper
    member this.Dot([<CallerMemberName;Optional;DefaultParameterValue("")>] memberName:string) = 
        Dot(this, memberName)

and IWrappable<^T when ^T :> Expr and ^T :> IWrappable<^T> > = 
    static abstract member Wrap: Op -> ^T

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
    | Bool of System.Boolean
    | Int of System.Int32
    | Float of System.Double
    // Binary Arithmetic
    | Unary of UnaryFn * Expr
    | Binary of BinaryFn * Expr * Expr
    | Ternary of TernaryFn * Expr * Expr * Expr
    | Comparison of ComparisonFn * Expr * Expr // Returns bool[N]
    // Special
    | IfThenElse of bool * Expr * Expr
    | Vector of Expr[]
    | Length of Expr
    | Dot of Expr * string

and [<Sealed>] bool =
    inherit Expr
    interface IWrappable<bool> with
        static member Wrap(op) = bool(op)
    static member WrapComparison(op:Op) = bool(op)

    internal new (op:Op) = { inherit Expr(op) }

and [<Sealed>] int =
    inherit Expr
    interface IWrappable<int> with
        static member Wrap(op:Op) = int(op)
    static member WrapComparison(op:Op) = bool(op)

    new (v) = { inherit Expr(Int(v)) }
    internal new (op:Op) = { inherit Expr(op) }
    static member op_Implicit(v:Int32) = int(v)

and [<Sealed>] float =
    inherit Expr
    interface IWrappable<float> with
        static member Wrap(op) = float(op)
    static member WrapComparison(op:Op) = bool(op)

    new (v:Double) = { inherit Expr(Float(v)) }
    new (v:Int32) = { inherit Expr(Float(v)) }
    internal new (v:Op) = { inherit Expr(v) }

    static member op_Implicit(v:Double) = float(v)
    static member op_Implicit(v:Int32) = float(v)

and [<Sealed>] vector2<^T when 'T:>IWrappable<'T> and 'T:>Expr > = 
    inherit Expr
    interface IWrappable<vector2<'T>> with
        static member Wrap(op:Op) = vector2(op)
    static member WrapComparison(op:Op) = vector2<bool>(op)

    new (v:'T) = { inherit Expr(Vector([|v;v|])) }
    new (x:'T,y:'T) = { inherit Expr(Vector([|x;y|])) }
    internal new (op:Op) = { inherit Expr(op) }

    member this.x = 'T.Wrap(this.Dot())
    member this.y = 'T.Wrap(this.Dot())

and [<Sealed>] vector3<'T when 'T:>IWrappable<'T> and 'T:>Expr> = 
    inherit Expr
    interface IWrappable<vector3<'T>> with
        static member Wrap(op:Op) = vector3(op)
    static member WrapComparison(op:Op) = vector3<bool>(op)

    new (v:'T) = { inherit Expr(Vector([|v;v;v|])) }
    new (x:'T,y:'T,z:'T) = { inherit Expr(Vector([|x;y;z|])) }
    internal new (op:Op) = { inherit Expr(op) }

    member this.x = 'T.Wrap(this.Dot())
    member this.y = 'T.Wrap(this.Dot())
    member this.z = 'T.Wrap(this.Dot())

and [<Sealed>] vector4<'T when 'T:>IWrappable<'T> and 'T:>Expr> = 
    inherit Expr
    interface IWrappable<vector4<'T>> with
        static member Wrap(op:Op) = vector4(op)
    static member WrapComparison(op:Op) = vector4<bool>(op)

    new (v:'T) = { inherit Expr(Vector([|v;v;v;v|])) }
    new (v:vector3<'T>,w:'T) = { inherit Expr(Vector([|v.x;v.y;v.z;w|])) }
    new (x:'T,y:'T,z:'T,w:'T) = { inherit Expr(Vector([|x;y;z;w|])) }
    internal new (op:Op) = { inherit Expr(op) }

    member this.x = 'T.Wrap(this.Dot())
    member this.y = 'T.Wrap(this.Dot())
    member this.z = 'T.Wrap(this.Dot())
    member this.w = 'T.Wrap(this.Dot())
    member this.xy = vector2<'T>(this.Dot())
    member this.zw = vector2<'T>(this.Dot())
    member this.xyz = vector3<'T>(this.Dot())
    member this.r = 'T.Wrap(this.Dot())
    member this.g = 'T.Wrap(this.Dot())
    member this.b = 'T.Wrap(this.Dot())
    member this.a = 'T.Wrap(this.Dot())
    member this.rgb = vector3<'T>(this.Dot())

type bool2 = vector2<bool>
type bool3 = vector3<bool>
type bool4 = vector4<bool>

type int2 = vector2<int>
type int3 = vector3<int>
type int4 = vector4<int>

type float2 = vector2<float>
type float3 = vector3<float>
type float4 = vector4<float>

// Create a typesafe wrapper for a generic Op e.g. float, float2, etc.
let inline Wrap<^T when ^T:>IWrappable<^T>> (op:Op) :^T = 'T.Wrap(op)

let inline abs (v:'T):'T = Wrap(Unary(Abs,v))
let inline length (v:'T):float = Wrap<float>(Length(v))
let inline max (v1:'T,v2:'T):'T = Wrap(Binary(Max,v1,v2))
let inline min (v1:'T,v2:'T):'T = Wrap(Binary(Min,v1,v2))

let inline (~+) (v:'T) :'T = Wrap(Unary(Plus,v))
let inline (~-) (v:'T) :'T = Wrap(Unary(Minus,v))
let inline (+) (v1:'T) (v2:'T) :'T = Wrap(Binary(Add,v1,v2))
let inline (-) (v1:'T) (v2:'T) :'T = Wrap(Binary(Sub,v1,v2))
let inline (*) (v1:'T) (v2:'T) :'T = Wrap(Binary(Mul,v1,v2))
let inline (/) (v1:'T) (v2:'T) :'T = Wrap(Binary(Div,v1,v2))

let inline WrapComparison<^T,^U when ^T:(static member WrapComparison: Op -> ^U)> (op:Op) : ^U = 'T.WrapComparison(op)

let inline (==) (v1:^T) (v2:^T) = WrapComparison<^T,^U>(Comparison(EQ,v1,v2))
let inline (<>) (v1:'T) (v2:'T) = WrapComparison<^T,^U>(Comparison(NE,v1,v2))
let inline (<) (v1:'T) (v2:'T) = WrapComparison<^T,^U>(Comparison(LT,v1,v2))
let inline (>) (v1:'T) (v2:'T) = WrapComparison<^T,^U>(Comparison(GT,v1,v2))
let inline (<=) (v1:'T) (v2:'T) = WrapComparison<^T,^U>(Comparison(LT,v1,v2))
let inline (>=) (v1:'T) (v2:'T) = WrapComparison<^T,^U>(Comparison(GE,v1,v2))


let v1 = float4(1,2,3,4)
let v2 = float4(1,2,3,5)
let c = (v1==v2)
let l = length v1

let f1 = float(1.0)
let f2 = float(2.0)
let d = (f1==f2)

// Syntax options for (if...then...else)
// If (condition) (trueValue,falseValue)
let inline If (cond:bool) (t:'T,f:'T) = Wrap<'T>(IfThenElse(cond,t,f))

let inline invLerp (v1:'T, v2:'T, v:'T) :'T =
    (v - v1) / (v2 - v1)
