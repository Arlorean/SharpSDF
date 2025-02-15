module SharpSDF.Compiler2

let compileToInterpreter (shader : Ast2.float2 -> Ast2.float) : (HLSL.float2 -> float) =
    fun (p : HLSL.float2) -> 
        (p.x,p.y) |> Ast2.float2 |> shader |> Interpreter2.EvalF

