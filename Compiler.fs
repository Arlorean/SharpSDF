module SharpSDF.Compiler

open SharpSDF.Ast

// type NativeShapeFn = SharpSDF.HLSL.float2 -> float

// type HlslSource = HlslSource of string
//     with
//         member __.Source = let (HlslSource s) = __ in s
//         override __.ToString() = __.Source
//         static member Of s = HlslSource s

// [<RequireQualifiedAccess>]
// type CompilationTarget =
//     | Interpreter
//     | HLSL
//     | JS
//     | IL

// [<RequireQualifiedAccess>]
// type CompilationOutput<'F> =
//     | Fn of 'F
//     | Hlsl of HlslSource 

// type CompilationResult = Result<CompilationOutput<NativeShapeFn>, string>

// Compile (Float2 -> Float) into a (float2 -> float)
let compileToInterpreter (shape : Float2 -> Float) : (HLSL.float2 -> float) =
    fun (p : HLSL.float2) -> 
        p |> f2 |> shape |> Interpreter.evaluateFloat1

// let compileToJS (shape : Float2 -> Float) : (HLSL.float2 -> float) =
//     (fun p -> p.x)

// let compileToIL (shape : Float2 -> Float) : (HLSL.float2 -> float) =
//     (fun p -> p.x)

// let compileToHLSL (shape : Float2 -> Float) : HlslSource =
//     HlslSource.Of "float shape(float2 p) { return float(0); }"

// let compileShape (target : CompilationTarget) (fn : Ast.ShapeFn) : CompilationResult =
//     match target with
//     | CompilationTarget.Interpreter -> fn |> compileToInterpreter |> CompilationOutput.Fn |> Ok
//     | CompilationTarget.HLSL -> Error "Not implemented"
//     | CompilationTarget.JS -> Error "Not implemented"
//     | CompilationTarget.IL -> Error "Not implemented"
