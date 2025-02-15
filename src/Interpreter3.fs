module SharpSDF.Interpreter3

open Ast3
open System

let inline length2 x y = Math.Sqrt(x*x + y*y)
let inline length3 x y z = Math.Sqrt(x*x + y*y + z*z)
let inline length4 x y z w = Math.Sqrt(x*x + y*y + z*z + w*w)
let inline lerp (v1:'T) (v2:'T) (t:'T) : 'T
    = v1 + t*(v2-v1)
let inline smoothstep min max v =
    let t = Math.Clamp((v - min) / (max - min), 0.0, 1.0)
    t*t*(3.0-2.0*t)
let inline step v1 v2 = if v1 >= v2 then 1.0 else 0.0

let rec EvalB(v:Bool) : Values.bool =
    let Eval = EvalB
    match v with
    | Bool.Varying s -> false // TODO: Lookup from context
    | Bool.Literal v -> v
    | Bool.IfThenElse (cond,t,f) -> if Eval cond then Eval t else Eval f
    | Bool.(!) v -> Eval v |> not
    | Bool.(&&) (v1, v2) -> Eval v1 && Eval v2
    | Bool.(||) (v1, v2) -> Eval v1 || Eval v2
    | Bool.(==) (v1, v2) -> Eval v1 =  Eval v2
    | Bool.(!=) (v1, v2) -> Eval v1 <> Eval v2
    | Bool.CompareInt (fn, v1, v2) ->
        (EvalI v1, EvalI v2) ||>
            match fn with
            | EQ -> (=)
            | NE -> (<>)
            | LT -> (<)
            | GT -> (>)
            | LE -> (<=)
            | GE -> (>=)
    | Bool.CompareFloat (fn, v1, v2) ->
        (EvalF v1, EvalF v2) ||>
            match fn with
            | EQ -> (=)
            | NE -> (<>)
            | LT -> (<)
            | GT -> (>)
            | LE -> (<=)
            | GE -> (>=)

and EvalI(v:Int) : Values.int =
    let Eval = EvalI
    match v with
    | Int.Varying s -> 0 // TODO: Lookup from context
    | Int.Literal v -> v
    | Int.IfThenElse (cond,t,f) -> if EvalB cond then Eval t else Eval f
    | Int.(~-) v -> -Eval v
    | Int.(~+) v -> +Eval v
    | Int.(+) (v1, v2) -> Eval v1 + Eval v2
    | Int.(-) (v1, v2) -> Eval v1 - Eval v2
    | Int.(*) (v1, v2) -> Eval v1 * Eval v2
    | Int.(/) (v1, v2) -> Eval v1 / Eval v2
    | Int.(%) (v1, v2) -> Eval v1 % Eval v2
    | Int.Abs v -> Math.Abs(Eval v)
    | Int.Clamp (v, min, max) -> Math.Clamp(Eval v, Eval min, Eval max)
    | Int.Max (v1, v2) -> Math.Max(Eval v1, Eval v2)
    | Int.Min (v1, v2) -> Math.Min(Eval v1, Eval v2)

and EvalF(v:Float) : Values.float =
    let Eval = EvalF
    match v with
    | Float.Varying s -> 0.0 // TODO: Lookup from context
    | Float.Literal v -> v
    | Float.Length2 (x, y) -> length2 (Eval x) (Eval y)
    | Float.Length3 (x, y, z) -> length3 (Eval x) (Eval y) (Eval z)
    | Float.Length4 (x, y, z, w) -> length4 (Eval x) (Eval y) (Eval z) (Eval w)
    | Float.IfThenElse (cond,t,f) -> if EvalB cond then Eval t else Eval f
    | Float.(~-) v -> -Eval v
    | Float.(~+) v -> +Eval v
    | Float.(+) (v1, v2) -> Eval v1 + Eval v2
    | Float.(-) (v1, v2) -> Eval v1 - Eval v2
    | Float.(*) (v1, v2) -> Eval v1 * Eval v2
    | Float.(/) (v1, v2) -> Eval v1 / Eval v2
    | Float.(%) (v1, v2) -> Eval v1 % Eval v2
    | Float.Abs v -> Math.Abs(Eval v)
    | Float.Clamp (v, min, max) -> Math.Clamp(Eval v, Eval min, Eval max)
    | Float.Exp v -> Math.Exp(Eval v)
    | Float.Lerp (v1, v2, t) -> lerp (Eval v1) (Eval v2) (Eval t)
    | Float.Max (v1, v2) -> Math.Max(Eval v1, Eval v2)
    | Float.Min (v1, v2) -> Math.Min(Eval v1, Eval v2)
    | Float.SmoothStep (min, max, v) -> smoothstep (Eval min) (Eval max) (Eval v)
    | Float.Step (v1, v2) -> step (Eval v1) (Eval v2)

let Eval (c:Wrappers.float4) = 
    Values.float4(EvalF c.x, EvalF c.y, EvalF c.z, EvalF c.w)

let compileToInterpreter (shader : Wrappers.float2 -> Wrappers.float4) =
    fun (p:Values.float2) -> 
        (p.x,p.y) |> Wrappers.float2 |> shader |> Eval
