module SharpSDF.Optimizer

open SharpSDF.Ast

let rec EvalB(v:Bool) : Bool =
    let Eval = EvalB

    let u =
        match v with       
        | Bool.Varying _ -> v 
        | Bool.Literal _ -> v 
        | Bool.IfThenElse (cond,t,f) -> Bool.IfThenElse (Eval cond, Eval t, Eval f)
        | Bool.(!) (v) -> Bool.(!) (Eval v)
        | Bool.(&&) (v1, v2) -> Bool.(&&) (Eval v1, Eval v2)
        | Bool.(||) (v1, v2) -> Bool.(||) (Eval v1, Eval v2)
        | Bool.(==) (v1, v2) -> Bool.(==) (Eval v1, Eval v2)
        | Bool.(!=) (v1, v2) -> Bool.(!=) (Eval v1, Eval v2)
        | Bool.CompareInt (fn, v1, v2) -> Bool.CompareInt (fn, EvalI v1, EvalI v2)
        | Bool.CompareFloat (fn, v1, v2) -> Bool.CompareFloat (fn, EvalF v1, EvalF v2)

    match u with
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
    | v -> v

and EvalI(v:Int) : Int =
    let Eval = EvalI

    let u =
        match v with
        | Int.Varying _ -> v 
        | Int.Literal _ -> v         
        | Int.IfThenElse (cond,t,f) -> Int.IfThenElse (EvalB cond, Eval t, Eval f) 
        | Int.(~-) (v) -> Int.(~-) (Eval v)
        | Int.(~+) (v) -> Int.(~+) (Eval v)
        | Int.(+) (v1, v2) -> Int.(+) (Eval v1, Eval v2) 
        | Int.(-) (v1, v2) -> Int.(-) (Eval v1, Eval v2) 
        | Int.(*) (v1, v2) -> Int.(*) (Eval v1, Eval v2) 
        | Int.(/) (v1, v2) -> Int.(/) (Eval v1, Eval v2) 
        | Int.(%) (v1, v2) -> Int.(%) (Eval v1, Eval v2) 
        | Int.Abs (v) -> Int.Abs (Eval v)
        | Int.Clamp (v, min, max) -> Int.Clamp (Eval v, Eval min, Eval max)
        | Int.Max (v1, v2) -> Int.Max (Eval v1, Eval v2)
        | Int.Min (v1, v2) -> Int.Min (Eval v1, Eval v2)
        
    match u with
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
    let Eval = EvalF

    let u =
        match v with
        | Float.Varying _ -> v
        | Float.Literal _ -> v
        | Float.Length2 (x,y) -> Float.Length2 (Eval x, Eval y)
        | Float.Length3 (x,y,z) -> Float.Length3 (Eval x, Eval y, Eval z)
        | Float.Length4 (x,y,z,w) -> Float.Length4 (Eval x, Eval y, Eval z, Eval w)
        | Float.IfThenElse (cond,t,f) -> Float.IfThenElse (EvalB cond, Eval t, Eval f)
        | Float.(~-) (v) -> Float.(~-) (Eval v)
        | Float.(~+) (v) -> Float.(~+) (Eval v)
        | Float.(+) (v1, v2) -> Float.(+)(Eval v1, Eval v2)
        | Float.(-) (v1, v2) -> Float.(-)(Eval v1, Eval v2)
        | Float.(*) (v1, v2) -> Float.(*)(Eval v1, Eval v2)
        | Float.(/) (v1, v2) -> Float.(/)(Eval v1, Eval v2)
        | Float.(%) (v1, v2) -> Float.(%)(Eval v1, Eval v2)
        | Float.Abs (v) -> Float.Abs (Eval v)
        | Float.Clamp (v, min, max) -> Float.Clamp(Eval v, Eval min, Eval max)
        | Float.Exp (v) -> Float.Exp (Eval v)
        | Float.Lerp (v1, v2, t) -> Float.Lerp (Eval v1, Eval v2, Eval t)
        | Float.Max (v1, v2) -> Float.Max (Eval v1, Eval v2)
        | Float.Min (v1, v2) -> Float.Min (Eval v1, Eval v2)
        | Float.SmoothStep (min, max, v) -> Float.SmoothStep (Eval min, Eval max, Eval v)
        | Float.Step (v1, v2) -> Float.Step (Eval v1, Eval v2)

    match u with
    | Float.Length2 (Float.Literal x, Float.Literal y) -> Float.Literal (Intrinsics.length(x,y))
    | Float.Length2 (x,y) -> Float.Length2 (Eval x, Eval y)
    | Float.Length3 (Float.Literal x, Float.Literal y, Float.Literal z) -> Float.Literal (Intrinsics.length(x,y,z))
    | Float.Length3 (x,y,z) -> Float.Length3 (Eval x, Eval y, Eval z)
    | Float.Length4 (Float.Literal x, Float.Literal y, Float.Literal z, Float.Literal w) -> Float.Literal (Intrinsics.length(x,y,z,w))
    | Float.Length4 (x,y,z,w) -> Float.Length4 (Eval x, Eval y, Eval z, Eval w)
    | Float.IfThenElse (Bool.Literal cond,t,f) -> if cond then t else f
    | Float.(~-) (Float.Literal v) -> Eval (Float.Literal -v)
    | Float.(~+) (Float.Literal v) -> Eval (Float.Literal +v)
    | Float.(+) (v1, Float.Literal 0.0) -> Eval v1
    | Float.(+) (Float.Literal 0.0, v2) -> Eval v2
    | Float.(+) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1+v2)
    | Float.(-) (v1, Float.Literal 0.0) -> Eval v1
    | Float.(-) (Float.Literal 0.0, v2) -> Float.(~-) (Eval v2)
    | Float.(-) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1-v2)
    | Float.(*) (_, Float.Literal 0.0) -> Float.Literal 0.0
    | Float.(*) (Float.Literal 0.0, _) -> Float.Literal 0.0
    | Float.(*) (v1, Float.Literal 1.0) -> Eval v1
    | Float.(*) (Float.Literal 1.0, v2) -> Eval v2
    | Float.(*) (Float.Literal v1, Float.Literal v2) -> Float.Literal (v1*v2)
    | Float.(/) (v1, Float.Literal 1.0) -> Eval v1
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
    | v -> v

let foldConstantExpressions (shader : Wrappers.float4) = 
    let color = shader
    let r = EvalF color.r
    let g = EvalF color.g
    let b = EvalF color.z
    let a = EvalF color.w
    Wrappers.float4(
        Wrappers.float r,
        Wrappers.float g,
        Wrappers.float b,
        Wrappers.float a
    )
