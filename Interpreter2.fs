module SharpSDF.Interpreter2

open System

[<RequireQualifiedAccessAttribute>]
type Value = 
    | Bool of bool
    | Int of int
    | Float of float
    | Vector of Value[]
with
    static member inline ERROR = raise (NotSupportedException())

    static member inline ToBool(v:Value) = 
        match v with
        | Bool v -> v
        | _ -> failwithf "%A is not a Bool" v

    static member inline op_Explicit(v:Value) = 
        match v with
        | Int v -> v
        | _ -> failwithf "%A is not a Int" v

    static member inline op_Explicit(v:Value) = 
        match v with
        | Float v -> v
        | _ -> failwithf "%A is not a Float" v


    static member Unary () (v:Value) = 
        match v with
        | Int v -> Int +v
        | Float v -> Float +v
        | Vector v -> Vector (Array.map (Value.Plus) v)
        | _ -> Value.ERROR

    static member Plus (v:Value) = 
        match v with
        | Int v -> Int +v
        | Float v -> Float +v
        | Vector v -> Vector (Array.map (Value.Plus) v)
        | _ -> Value.ERROR

    static member Minus (v:Value) = 
        match v with
        | Int v -> Int -v
        | Float v -> Float -v
        | Vector v -> Vector (Array.map (Value.Minus) v)
        | _ -> Value.ERROR

    static member Abs (v:Value) = 
        match v with
        | Int v -> Int (Math.Abs(v))
        | Float v -> Float (Math.Abs(v))
        | Vector v -> Vector (Array.map (Value.Abs) v)
        | _ -> Value.ERROR

    static member Exp (v:Value) = 
        match v with
        | Int v -> Int (int (Math.Exp((float v)))) // FIXME: Is this correct?
        | Float v -> Float (Math.Exp(v))
        | Vector v -> Vector (Array.map Value.Exp v)
        | _ -> Value.ERROR

    static member Add (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (v1+v2)
        | Float v1, Float v2 -> Float (v1+v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Add v1 v2)
        | _ -> Value.ERROR

    static member Sub (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (v1-v2)
        | Float v1, Float v2 -> Float (v1-v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Sub v1 v2)
        | _ -> Value.ERROR

    static member Mul (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (v1*v2)
        | Float v1, Float v2 -> Float (v1*v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Mul v1 v2)
        | _ -> Value.ERROR

    static member Div (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (v1/v2)
        | Float v1, Float v2 -> Float (v1/v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Div v1 v2)
        | _ -> Value.ERROR

    static member Rem (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (v1%v2)
        | Float v1, Float v2 -> Float (v1%v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Rem v1 v2)
        | _ -> Value.ERROR

    static member Min (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (Math.Min(v1,v2))
        | Float v1, Float v2 -> Float (Math.Min(v1,v2))
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Min v1 v2)
        | _ -> Value.ERROR

    static member Max (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (Math.Max(v1,v2))
        | Float v1, Float v2 -> Float (Math.Max(v1,v2))
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Max v1 v2)
        | _ -> Value.ERROR

    static member Step (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Int v1, Int v2 -> Int (if v1 >= v2 then 1 else 0)
        | Float v1, Float v2 -> Float (if v1 >= v2 then 1.0 else 0.0)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.Step v1 v2)
        | _ -> Value.ERROR

    static member EQ (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1=v2)
        | Int v1, Int v2 -> Bool (v1=v2)
        | Float v1, Float v2 -> Bool (v1=v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.EQ v1 v2)
        | _ -> Value.ERROR

    static member NE (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1<>v2)
        | Int v1, Int v2 -> Bool (v1<>v2)
        | Float v1, Float v2 -> Bool (v1<>v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.NE v1 v2)
        | _ -> Value.ERROR

    static member LT (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1<v2)
        | Int v1, Int v2 -> Bool (v1<v2)
        | Float v1, Float v2 -> Bool (v1<v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.LT v1 v2)
        | _ -> Value.ERROR

    static member GT (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1>v2)
        | Int v1, Int v2 -> Bool (v1>v2)
        | Float v1, Float v2 -> Bool (v1>v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.GT v1 v2)
        | _ -> Value.ERROR

    static member LE (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1<=v2)
        | Int v1, Int v2 -> Bool (v1<=v2)
        | Float v1, Float v2 -> Bool (v1<=v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.LE v1 v2)
        | _ -> Value.ERROR

    static member GE (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1>=v2)
        | Int v1, Int v2 -> Bool (v1>=v2)
        | Float v1, Float v2 -> Bool (v1>=v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.GE v1 v2)
        | _ -> Value.ERROR

    static member AND (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1&&v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.AND v1 v2)
        | _ -> Value.ERROR

    static member OR (v1:Value) (v2:Value) = 
        match (v1,v2) with
        | Bool v1, Bool v2 -> Bool (v1||v2)
        | Vector v1, Vector v2 -> Vector (Array.map2 Value.OR v1 v2)
        | _ -> Value.ERROR

    static member Clamp (v:Value) (min:Value) (max:Value) = 
        match (v,min,max) with
        | Int v, Int min, Int max -> Int (Math.Clamp(v,min,max))
        | Float v, Float min, Float max -> Float (Math.Clamp(v,min,max))
        | Vector v, Vector min, Vector max -> Vector (Array.map3 Value.Clamp v min max)
        | _ -> Value.ERROR

    static member Lerp (v1:Value) (v2:Value) (t:Value) = 
        match (v1,v2,t) with
        | Float v1, Float v2, Float t -> Float (v1 + t*(v2-v1))
        | Vector v1, Vector v2, Vector t -> Vector (Array.map3 Value.Lerp v1 v2 t)
        | _ -> Value.ERROR

    static member Smoothstep (min:Value) (max:Value) (v:Value) = 
        match (min,max,v) with
        | Float min, Float max, Float v ->  
            let t = Math.Clamp ((v - min) / (max - min), 0.0, 1.0)
            Float (t*t*(3.0-2.0*t))
        | Vector min, Vector max, Vector v -> Vector (Array.map3 Value.Smoothstep min max v)
        | _ -> Value.ERROR

    static member Swizzle (v:Value[]) s = 
        match s with
        | "x" | "r" -> v[0]
        | "y" | "g" -> v[1]
        | "z" | "b" -> v[2]
        | "w" | "a" -> v[3]
        | "xy" | "rg" -> Vector [|v[0];v[1]|]
        | "xyz" | "rgb" -> Vector [|v[0];v[1];v[2]|]
        | _ -> Value.ERROR

    static member Dot (s:string) (v:Value) = 
        match v with
        | Vector v -> Value.Swizzle v s
        | _ -> Value.ERROR

    static member Length (v:Value) =
        match v with
        | Vector v ->
            Array.map (fun v -> float v * float v) v
            |> Array.sum 
            |> Math.Sqrt
            |> Float
        | _ -> failwithf "%A is not a Vector" v

type Operator =
    static member internal Fn(fn:Ast2.UnaryFn) = 
        match fn with
        | Ast2.Plus -> Value.Plus
        | Ast2.Minus -> Value.Minus
        | Ast2.Abs -> Value.Abs
        | Ast2.Exp -> Value.Exp
    static member internal Fn(fn:Ast2.BinaryFn) = 
        match fn with
        | Ast2.Add -> Value.Add
        | Ast2.Sub -> Value.Sub
        | Ast2.Mul -> Value.Mul
        | Ast2.Div -> Value.Div
        | Ast2.Rem -> Value.Rem
        | Ast2.Min -> Value.Min
        | Ast2.Max -> Value.Max
        | Ast2.Step -> Value.Step
        | Ast2.EQ -> Value.EQ
        | Ast2.NE -> Value.NE
        | Ast2.LT -> Value.LT
        | Ast2.GT -> Value.GT
        | Ast2.LE -> Value.LE
        | Ast2.GE -> Value.GE
        | Ast2.AND -> Value.AND
        | Ast2.OR -> Value.OR
    static member internal Fn(fn:Ast2.TernaryFn) = 
        match fn with
        | Ast2.Clamp -> Value.Clamp
        | Ast2.Lerp -> Value.Lerp
        | Ast2.Smoothstep -> Value.Smoothstep

let rec Eval(expr:Ast2.Expr):Value =
    match expr.op with
    | Ast2.Bool (v) -> Value.Bool v
    | Ast2.Int (v) -> Value.Int v
    | Ast2.Float (v) -> Value.Float v
    | Ast2.Vector (v) -> Value.Vector (Array.map Eval v)
    | Ast2.Length (v) -> Eval(v) |> Value.Length
    | Ast2.Unary (op, v) -> Eval(v) |> (Operator.Fn op)
    | Ast2.Binary (op, v1, v2) -> (Eval(v1), Eval(v2)) ||> (Operator.Fn op)
    | Ast2.Ternary (op, v1, v2, v3) -> (Eval(v1), Eval(v2), Eval(v3)) |||> (Operator.Fn op)
    | Ast2.IfThenElse (cond, t, f) -> if (Eval(cond)|>Value.ToBool) then Eval(t) else Eval(f)
    | Ast2.Dot (v, s) -> Eval(v) |> Value.Dot s

let compileToInterpreter (shader : Ast2.float2 -> Ast2.float4) : (HLSL.float2 -> HLSL.float4) =
    fun (p : HLSL.float2) -> 
        let v = (p.x,p.y) |> Ast2.float2 |> shader |> Eval
        match v with
        | Value.Vector [|Value.Float x;Value.Float y;Value.Float z;Value.Float w|] -> HLSL.float4(x,y,z,w)
        | _ -> failwithf "%A is not a HLSL.float4" v
