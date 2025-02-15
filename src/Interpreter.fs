module SharpSDF.Interpreter

open SharpSDF.Ast

let rec evaluateFloat1 (expr : Float) : float =
    let f1 = evaluateFloat1
    let f2 = evaluateFloat2
    let b1  = evaluateBool

    match expr with
    | Float.Float (a) -> a
    | Float.Sub (a,b) ->
        f1 a - f1 b
    | Float.Add (a,b) ->
        f1 a + f1 b
    | Float.Div (a,b) ->
        f1 a / f1 b
    | Float.Min (a,b) ->
        HLSL.Intrinsics.min( f1 a, f1 b )
    | Float.Max (a,b) ->
        HLSL.Intrinsics.max( f1 a, f1 b )
    | Float.Length a ->
        a |> f2 |> HLSL.Intrinsics.length
    | Float.Abs a ->
        a |> f1 |> HLSL.Intrinsics.abs
    | Float.X a ->
        a |> f2 |> _.x
    | Float.Y a ->
        a |> f2 |> _.y
    | Float.If (c, a, b) ->
        if (b1 c) then (f1 a) else (f1 b)
    | _ ->
        failwithf "Not implemented: %A" expr

and evaluateFloat2 (expr : Float2) : HLSL.float2 =
    let f1 = evaluateFloat1
    let f2 = evaluateFloat2
    let b1  = evaluateBool

    match expr with
    | Float2.Float2 (a,b) ->
        HLSL.float2(a |> f1 ,b |> f1)
    | Float2.Mul (a,b) ->
        (a |> f2) *  (b |> f1)
    | Float2.Add2 (a,b) ->
        (a |> f2) +  (b |> f2)
    | Float2.Sub2 (a,b) ->
        (a |> f2) -  (b |> f2)
    | Float2.Min (a,b) ->
        HLSL.Intrinsics.min( f2 a, f2 b )
    | Float2.Max (a,b) ->
        HLSL.Intrinsics.max( f2 a, f2 b )
    | Float2.If (c, a, b) ->
        if (b1 c) then (f2 a) else (f2 b)
    | _ ->
        failwithf "Not implemented: %A" expr

and evaluateBool (expr : Bool) : bool =
    let f1 = evaluateFloat1
    let f2 = evaluateFloat2
    let b1  = evaluateBool

    match expr with
    | Bool.Lt (a,b) ->
        (f1 a) < (f1 b)
    | Bool.Le (a,b) ->
        (f1 a) <= (f1 b)
    | Bool.Gt (a,b) ->
        (f1 a) > (f1 b)
    | Bool.Ge (a,b) ->
        (f1 a) >= (f1 b)
    | _ ->
        failwithf "Not implemented: %A" expr
