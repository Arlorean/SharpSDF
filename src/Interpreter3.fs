module SharpSDF.Interpreter3

open SharpSDF.Ast

let compileToInterpreter (shader : Wrappers.float4) =
    fun (p:Values.float2) -> 
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
            | Int.Abs v -> abs(Eval v)
            | Int.Clamp (v, min, max) -> Intrinsics.clamp(Eval v, Eval min, Eval max)
            | Int.Max (v1, v2) -> Intrinsics.max(Eval v1, Eval v2)
            | Int.Min (v1, v2) -> Intrinsics.min(Eval v1, Eval v2)

        and EvalF(v:Float): Values.float =
            let Eval = EvalF
            match v with
            | Float.Varying "p.x" -> p.x
            | Float.Varying "p.y" -> p.y
            | Float.Literal v -> v
            | Float.Length2 (x, y) -> Intrinsics.length(Eval x, Eval y)
            | Float.Length3 (x, y, z) -> Intrinsics.length(Eval x, Eval y, Eval z)
            | Float.Length4 (x, y, z, w) -> Intrinsics.length(Eval x, Eval y, Eval z, Eval w)
            | Float.IfThenElse (cond,t,f) -> if EvalB cond then Eval t else Eval f
            | Float.(~-) v -> -Eval v
            | Float.(~+) v -> +Eval v
            | Float.(+) (v1, v2) -> Eval v1 + Eval v2
            | Float.(-) (v1, v2) -> Eval v1 - Eval v2
            | Float.(*) (v1, v2) -> Eval v1 * Eval v2
            | Float.(/) (v1, v2) -> Eval v1 / Eval v2
            | Float.(%) (v1, v2) -> Eval v1 % Eval v2
            | Float.Abs v -> abs(Eval v)
            | Float.Clamp (v, min, max) -> Intrinsics.clamp(Eval v, Eval min, Eval max)
            | Float.Exp v -> exp(Eval v)
            | Float.Lerp (v1, v2, t) -> Intrinsics.lerp (Eval v1, Eval v2, Eval t)
            | Float.Max (v1, v2) -> Intrinsics.max(Eval v1, Eval v2)
            | Float.Min (v1, v2) -> Intrinsics.min(Eval v1, Eval v2)
            | Float.SmoothStep (min, max, v) -> Intrinsics.smoothstep (Eval min, Eval max, Eval v)
            | Float.Step (v1, v2) -> Intrinsics.step (Eval v1, Eval v2)
            | _ -> failwithf "Unknown Float value: %A" v

        let Eval (c:Wrappers.float4) = 
            Values.float4(EvalF c.x, EvalF c.y, EvalF c.z, EvalF c.w)

        shader |> Eval

