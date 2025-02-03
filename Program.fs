﻿open SharpSDF

let shader1 = TestShader.shader

let shader2 p = Colors.red

let main() =
#if FABLE_COMPILER
    // let webGL = new WebGLRenderer.WebGLRenderer("#WebGLRenderer")
    // let shaderSource = ShaderGenerator.makeShader TestShader.shader
    // webGL.RenderSource( shaderSource )

    let canvas = new CanvasRenderer.CanvasRenderer("#CanvasRenderer")
    canvas.Render shader1
#else
    let compiledShader = Interpreter2.compileToInterpreter shader1
    // let result = compiledShader (HLSL.float2(150,150))
    // printf "%A" result

    let console = new ConsoleRenderer.ConsoleRenderer(300, 300)
    console.Render compiledShader
    console.SavePNG "ConsoleRenderer.png"
#endif

main()
