﻿open SharpSDF

let shader = TestShader.shader

let main() =
    // Run optimization passes
    let shader = Optimizer.foldConstantExpressions shader
    //let shader = Optimizer.subExpressionElimination shader

    let interpretedShader = Interpreter3.compileToInterpreter shader
#if FABLE_COMPILER
    let webGL = new WebGLRenderer.WebGLRenderer("#WebGLRenderer")
    let shaderSource = GLSL.GenerateFragment shader
    webGL.RenderSource( shaderSource )
    printfn "%s" shaderSource

    let canvas = new CanvasRenderer.CanvasRenderer("#CanvasRenderer")
    canvas.Render interpretedShader
#else
    let dot = DOT.GenerateGraph shader
    System.IO.File.WriteAllText("TestShader.dot", dot)
    
    let console = new ConsoleRenderer.ConsoleRenderer(300, 300)
    console.Render interpretedShader
    console.SavePNG "docs/images/ConsoleRenderer.png"
#endif

main()
