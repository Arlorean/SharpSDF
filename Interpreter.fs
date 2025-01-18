module SharpSDF.Interpreter

open SharpSDF.Ast
open type SharpSDF.Ast.Intrinsics


let rec evaluateFloat1 (expr : Float) : float =
    match expr with
    | Float.Float (a) -> a
    | Float.Sub (a,b) ->
        evaluateFloat1 a - evaluateFloat1 b
    | Float.Add (a,b) ->
        evaluateFloat1 a + evaluateFloat1 b
    | Float.Min (a,b) ->
        HLSL.Intrinsics.min( evaluateFloat1 a, evaluateFloat1 b )
    | Float.Max (a,b) ->
        HLSL.Intrinsics.max( evaluateFloat1 a, evaluateFloat1 b )
    | Float.Length a ->
        a |> evaluateFloat2 |> HLSL.Intrinsics.length
    | Float.Abs a ->
        a |> evaluateFloat1 |> HLSL.Intrinsics.abs
    | Float.X a ->
        a |> evaluateFloat2 |> _.x
    | Float.Y a ->
        a |> evaluateFloat2 |> _.y
    | Float.If (c, a, b) ->
        if (evaluateBool c) then (evaluateFloat1 a) else (evaluateFloat1 b)
    | _ ->
        failwithf "Not implemented: %A" expr

and evaluateFloat2 (expr : Float2) : HLSL.float2 =
    match expr with
    | Float2.Float2 (a,b) ->
        HLSL.float2(a |> evaluateFloat1 ,b |> evaluateFloat1)
    | Float2.Add2 (a,b) ->
        (a |> evaluateFloat2) +  (b |> evaluateFloat2)
    | Float2.Sub2 (a,b) ->
        (a |> evaluateFloat2) -  (b |> evaluateFloat2)
    | Float2.Min (a,b) ->
        HLSL.Intrinsics.min( evaluateFloat2 a, evaluateFloat2 b )
    | Float2.Max (a,b) ->
        HLSL.Intrinsics.max( evaluateFloat2 a, evaluateFloat2 b )
    | Float2.If (c, a, b) ->
        if (evaluateBool c) then (evaluateFloat2 a) else (evaluateFloat2 b)
    | _ ->
        failwithf "Not implemented: %A" expr

and evaluateBool (expr : Bool) : bool =
    match expr with
    | Bool.Lt (a,b) ->
        (evaluateFloat1 a) < (evaluateFloat1 b)
    | Bool.Le (a,b) ->
        (evaluateFloat1 a) <= (evaluateFloat1 b)
    | Bool.Gt (a,b) ->
        (evaluateFloat1 a) > (evaluateFloat1 b)
    | Bool.Ge (a,b) ->
        (evaluateFloat1 a) >= (evaluateFloat1 b)
    | _ ->
        failwithf "Not implemented: %A" expr

// // Compile (Float2 -> Float) into a (float2 -> float)
// let compileToInterpreter (shape : Float2 -> Float) : (HLSL.float2 -> float) =
//     fun (p : HLSL.float2) -> 
//         p |> f2 |> shape |> evaluateFloat1

// let compileToJS (shape : Float2 -> Float) : (HLSL.float2 -> float) =
//     fun (p : HLSL.float2) -> 
//         p |> f2 |> shape |> evaluateFloat1

// let compileToIL (shape : Float2 -> Float) : (HLSL.float2 -> float) =
//     fun (p : HLSL.float2) -> 
//         p |> f2 |> shape |> evaluateFloat1

// type HlslSource = HlslSource of string
//     with
//         member __.Source = let (HlslSource s) = __ in s
//         override __.ToString() = __.Source
//         static member Of s = HlslSource s

// let compileToHLSL (shape : Float2 -> Float) : HlslSource =
//     HlslSource.Of "float shape(float2 p) { return float(0); }"

// // Referenced from test shader
// let testShape : HLSL.float2 -> float = 
//     (sdCircle (f1 100.0)) |> compileToInterpreter

