open SharpSDF

let main() =
#if FABLE_COMPILER
    let webGL = new WebGLRenderer.WebGLRenderer("#WebGLRenderer")
    let shaderSource = ShaderGenerator.makeShader TestShader.shader
    webGL.RenderSource( shaderSource )

    let canvas = new CanvasRenderer.CanvasRenderer("#CanvasRenderer")
    canvas.Render TestShader.shader
#else
    let console = new ConsoleRenderer.ConsoleRenderer(300, 300)
    console.Render TestShader.shader
    console.SavePNG "ConsoleRenderer.png"
#endif

main()
