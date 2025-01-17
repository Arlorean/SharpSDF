module SharpSDF.Quotation


module Shapes =

    open SharpSDF.HLSL
    open type Intrinsics

    let inline sdCircle r =
        fun (p:float2) -> length(p) - r
    
module Generator =
    open SharpSDF.HLSL
    open type Intrinsics

    open Microsoft.FSharp.Quotations
    
    open Microsoft.FSharp.Quotations.Patterns
    
    let rec printExpr expr : string =
        match expr with
        | Application (func, arg) ->
            sprintf "Application: %s( %s )" (printExpr func) (printExpr arg)
        // | Call (_, methodInfo, args) ->
        //     printfn "Call to method: %s" methodInfo.Name
        //     args |> List.iter printExpr
        | Value (v, t) ->
            sprintf "Value: %A (Type: %s)" v t.FullName
        | Var v ->
            sprintf "Variable: %s (Type: %s)" v.Name v.Type.FullName
        | _ ->
            sprintf "Other expression: %A" expr    
    
    let generate ( expr : Expr<float2 -> float> ) : string =
        printExpr expr
        
let main() =
    let shape = Shapes.sdCircle 10.0
    printfn "%s" (Generator.generate <@ shape @>)
    

let inline shader x = x + 2

let quotedShaderApplication = <@ shader 3 @>


open Microsoft.FSharp.Quotations

open Microsoft.FSharp.Quotations.Patterns
// Introspect the quotation
let rec printExpr expr =
    match expr with
    | Application (func, arg) ->
        printfn "Application:"
        printfn "  Function:"
        printExpr func
        printfn "  Argument:"
        printExpr arg
    | Call (_, methodInfo, args) ->
        printfn "Call to method: %s" methodInfo.Name
        args |> List.iter printExpr
    | Value (v, t) ->
        printfn "Value: %A (Type: %s)" v t.FullName
    | Var v ->
        printfn "Variable: %s (Type: %s)" v.Name v.Type.FullName
    | _ ->
        printfn "Other expression: %A" expr

//printExpr quotedShaderApplication    
main()

