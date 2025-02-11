module SharpSDF.ConsoleRenderer


type ConsoleRenderer(width:int, height:int) =
    let pixels = Array.create<byte> (width*height*4) 0uy

    member private this.SetPixel (p:HLSL.float2) (c:HLSL.float4) =
        let index = int32 (int32(p.y) * width + int32(p.x))*4
        pixels.[index + 0] <- byte (c.r*255.0)
        pixels.[index + 1] <- byte (c.g*255.0)
        pixels.[index + 2] <- byte (c.b*255.0)
        pixels.[index + 3] <- byte (c.a*255.0)
        ()

    member this.Render (shader : HLSL.float2 -> HLSL.float4) = 
        let size = HLSL.float2(width,height)
        let halfSize = size * 0.5
        for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let pixel = HLSL.float2(x, y)
            let position = HLSL.float2(x, height-y) - halfSize
            let color = shader position 
            this.SetPixel pixel color

#if !FABLE_COMPILER
    member this.SavePNG filename = 
        PNG.write pixels width height filename
#endif