module SharpSDF.Interpreter2

open SharpSDF.Ast2

let inline castTo<'T> (value: obj) : 'T =
    match box value with
    | :? 'T as result -> result
    | _ -> failwith "Invalid cast"


let bp (expr:Expr) = 
    printf "evaluate(%A)" expr
   

let rec inline evaluate<'T>(expr:Expr):'T =
    bp(expr)
    match expr.op with
    | Float (v) -> castTo<'T>(v)
    | Bool (v) -> castTo<'T>(v)
    | Int (v) -> castTo<'T>(v)
    // | Min (a,b) -> HLSL.Intrinsics.min(evaluate<'T>(a), evaluate<'T>(b))
    | _ -> failwithf "Not implemented: %A" expr
