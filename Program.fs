open SharpSDF
open SharpSDF.Colors

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

    let renderer = new SharpSDF.WebGLRenderer.WebGLRenderer(".view")

    let shaderSource = ShaderGenerator.makeShader shader

    renderer.RenderSource( shaderSource )

main()
