module SharpSDF.Interpreter2

open System
   
type Intrinsics =
    static member internal Abs(v:float) = Math.Abs(v)
    static member internal Clamp (v:float) min max = Math.Clamp(v,min,max)
    static member internal Exp(v:float) = Math.Exp(v)
    static member internal Lerp v1 v2 (t:float) = v1 + t*(v2-v1)
    static member internal Min(v1:float) (v2:float) = Math.Min(v1, v2)
    static member internal Max(v1:float) (v2:float) = Math.Max(v1, v2)
    static member internal Smoothstep min max (v:float) =
        let t = Intrinsics.Clamp ((v - min) / (max - min)) 0.0 1.0
        t*t*(3.0-2.0*t)
    static member internal Step(v1:float) (v2:float) = if v1 >= v2 then 1.0 else 0.0


type Interpreter =
    static member internal Fn(fn:Ast2.UnaryFn) = 
        match fn with
        | Ast2.Plus -> (~+)
        | Ast2.Minus -> (~-)
        | Ast2.Abs -> Intrinsics.Abs
        | Ast2.Exp -> Intrinsics.Exp
    static member internal Fn(fn:Ast2.BinaryFn) = 
        match fn with
        | Ast2.Add -> (+)
        | Ast2.Sub -> (-)
        | Ast2.Mul -> (*)
        | Ast2.Div -> (/)
        | Ast2.Rem -> (%)
        | Ast2.Min -> Intrinsics.Min
        | Ast2.Max -> Intrinsics.Max
        | Ast2.Step -> Intrinsics.Step
    static member internal Fn(fn:Ast2.TernaryFn) = 
        match fn with
        | Ast2.Clamp -> Intrinsics.Clamp
        | Ast2.Lerp -> Intrinsics.Lerp
        | Ast2.Smoothstep -> Intrinsics.Smoothstep
    static member internal Fn(fn:Ast2.ComparisonFn) = 
        match fn with
        | Ast2.EQ -> (=)
        | Ast2.NE -> (<>)
        | Ast2.LT -> (<)
        | Ast2.GT -> (>)
        | Ast2.LE -> (<=)
        | Ast2.GE -> (>=)
    static member internal Length v =
        v
        |> Array.map (fun x -> x * x)
        |> Array.sum
        |> Math.Sqrt

open type Interpreter

let rec EvalF(expr:Ast2.Expr):float =
    match expr.op with
    | Ast2.Float (v) -> v
    | Ast2.Unary (op, v) -> EvalF(v) |> (Fn op)
    | Ast2.Binary (op, v1, v2) -> (EvalF(v1), EvalF(v2)) ||> (Fn op)
    | Ast2.Ternary (op, v1, v2, v3) -> (EvalF(v1), EvalF(v2), EvalF(v3)) |||> (Fn op)
    | Ast2.Length (v) -> EvalFV(v) |> Length
    | _ -> failwithf "Not implemented: %A" expr.op
and EvalFV(expr:Ast2.Expr):float[] =
    match expr.op with
    | Ast2.Unary (op, v) -> EvalFV(v) |> Array.map (Fn op)
    | Ast2.Binary (op, v1, v2) -> (EvalFV(v1), EvalFV(v2)) ||> (Array.map2 (Fn op))
    | Ast2.Ternary (op, v1, v2, v3) -> (EvalFV(v1), EvalFV(v2), EvalFV(v3)) |||> (Array.map3 (Fn op))
    | Ast2.Vector (v) -> v |> Array.map (EvalF)
    | _ -> failwithf "Not implemented: %A" expr.op

