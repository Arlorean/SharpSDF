open SharpSDF
open SharpSDF.Colors
open SharpSDF.Renderer


let main() =
    let sdfShader =
        outerShadow gray50 30.0 |>>
        solidFill blue |>> 
        innerShadow gray50 30.0 |>> 
        solidStroke red 20.0
     
    let sdfShape =
        (_circle 100.0 |> _sdf)
    
    let background = clear

    let shader position =
        (position |> SdfContext.Create |> sdfShape |> sdfShader background) 

    // ShaderFn = float2 -> float4
    // ShaderSource = string (HLSL source)

    // Create:
    // ShaderFn -> ShaderSource

    // let renderer = CanvasRenderer(".view")
    // renderer.Render shader

    let renderer = new SharpSDF.WebGLRenderer.WebGLRenderer(".view")
    renderer.RenderSource( """
    void main() {
        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); // Red color
    }
    """)

main()
