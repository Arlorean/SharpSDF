module SharpSDF.Optimizer

open SharpSDF.Ast

let rec EvalB(v:Bool) : Bool =
    match v with
    | Bool.IfThenElse (Bool.Literal cond,t,f) -> if cond then t else f
    | Bool.(!) (Bool.Literal v) -> Bool.Literal (not v)
    | Bool.(&&) (Bool.Literal v1, Bool.Literal v2) -> Bool.Literal (v1&&v2)
    | Bool.(||) (Bool.Literal v1, Bool.Literal v2) -> Bool.Literal (v1||v2)
    | Bool.(==) (Bool.Literal v1, Bool.Literal v2) -> Bool.Literal (v1=v2)
    | Bool.(!=) (Bool.Literal v1, Bool.Literal v2) -> Bool.Literal (v1<>v2)
    | Bool.CompareInt (fn, Int.Literal v1, Int.Literal v2) -> 
        match fn with
        | EQ -> Bool.Literal (v1=v2)
        | NE -> Bool.Literal (v1<>v2)
        | LT -> Bool.Literal (v1<v2)
        | GT -> Bool.Literal (v1>v2)
        | LE -> Bool.Literal (v1<=v2)
        | GE -> Bool.Literal (v1>=v2)
    | Bool.CompareFloat (fn, Float.Literal v1, Float.Literal v2) ->
        match fn with
        | EQ -> Bool.Literal (v1=v2)
        | NE -> Bool.Literal (v1<>v2)
        | LT -> Bool.Literal (v1<v2)
        | GT -> Bool.Literal (v1>v2)
        | LE -> Bool.Literal (v1<=v2)
        | GE -> Bool.Literal (v1>=v2)
    | _ -> v

and EvalI(v:Int) : Int =
    match v with
    | Int.IfThenElse (Bool.Literal cond,t,f) -> if cond then t else f
    | Int.(~-) (Int.Literal v) -> Int.Literal -v
    | Int.(~+) (Int.Literal v) -> Int.Literal +v
    | Int.(+) (Int.Literal v1, Int.Literal v2) -> Int.Literal (v1+v2)
    | Int.(-) (Int.Literal v1, Int.Literal v2) -> Int.Literal (v1-v2)
    | Int.(*) (Int.Literal v1, Int.Literal v2) -> Int.Literal (v1*v2)
    | Int.(/) (Int.Literal v1, Int.Literal v2) -> Int.Literal (v1/v2)
    | Int.(%) (Int.Literal v1, Int.Literal v2) -> Int.Literal (v1%v2)
    | Int.Abs (Int.Literal v) -> Int.Literal (Intrinsics.abs v)
    | Int.Clamp (Int.Literal v, Int.Literal min, Int.Literal max) -> Int.Literal (Intrinsics.clamp(v, min, max))
    | Int.Max (Int.Literal v1, Int.Literal v2) -> Int.Literal (Intrinsics.max(v1,v2))
    | Int.Min (Int.Literal v1, Int.Literal v2) -> Int.Literal (Intrinsics.min(v1,v2))
    | _ -> v

and EvalF(v:Float) : Float =
    match v with
    | Float.Length2 (Float.Literal x, Float.Literal y) -> Float.Literal (Intrinsics.length(x,y))
    | Float.Length3 (Float.Literal x, Float.Literal y, Float.Literal z) -> Float.Literal (Intrinsics.length(x,y,z))
    | Float.Length4 (Float.Literal x, Float.Literal y, Float.Literal z, Float.Literal w) -> Float.Literal (Intrinsics.length(x,y,z,w))
    | Float.IfThenElse (Bool.Literal cond,t,f) -> if cond then t else f
    | Float.(~-) (Float.Literal v) -> Float.Literal -v
    | Float.(~+) (Float.Literal v) -> Float.Literal +v
    | Float.(+) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1+v2)
    | Float.(-) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1-v2)
    | Float.(*) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1*v2)
    | Float.(/) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1/v2)
    | Float.(%) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1%v2)
    | Float.Abs (Float.Literal v) -> Float.Literal (Intrinsics.abs v)
    | Float.Clamp (Float.Literal v, Float.Literal min, Float.Literal max) -> Float.Literal (Intrinsics.clamp(v, min, max))
    | Float.Exp (Float.Literal v) -> Float.Literal (Intrinsics.exp v)
    | Float.Lerp (Float.Literal v1, Float.Literal v2, Float.Literal t) -> Float.Literal (Intrinsics.lerp(v1,v2,t))
    | Float.Max (Float.Literal v1, Float.Literal v2) -> Float.Literal (Intrinsics.max(v1,v2))
    | Float.Min (Float.Literal v1, Float.Literal v2) -> Float.Literal (Intrinsics.min(v1,v2))
    | Float.SmoothStep (Float.Literal min, Float.Literal max, Float.Literal v) -> Float.Literal (Intrinsics.smoothstep(min,max,v))
    | Float.Step (Float.Literal v1, Float.Literal v2) -> Float.Literal (Intrinsics.min(v1,v2))
    | _ -> v

let foldConstantExpressions (shader : Wrappers.float2 -> Wrappers.float4) = 
    let p = Wrappers.float2(Wrappers.float(Float.Varying "p.x"), Wrappers.float(Float.Varying "p.y"))
    let color = p |> shader
    let r = EvalF color.r
    let g = EvalF color.g
    let b = EvalF color.z
    let a = EvalF color.w
    // TODO: Create a function where p is changed back into a parameter
    shader
