module SharpSDF.CanvasRenderer

open Browser.Types
open Browser

open HLSL

type CanvasRenderer =
    val private canvas: HTMLCanvasElement
    val private context: CanvasRenderingContext2D
    val private imageData: ImageData

    new (canvasId:string) = 
        let canvas = document.querySelector(canvasId) :?> HTMLCanvasElement
        let context = canvas.getContext_2d()
        let imageData = context.createImageData(int canvas.width,int canvas.height)
        { canvas=canvas; context=context; imageData=imageData }

    member private this.SetPixel (imageData:ImageData) (p:float2) (c:float4) =
        let index = int (p.y * imageData.width + p.x) * 4
        imageData.data.[index + 0] <- uint8 (c.r*255.0)
        imageData.data.[index + 1] <- uint8 (c.g*255.0)
        imageData.data.[index + 2] <- uint8 (c.b*255.0)
        imageData.data.[index + 3] <- uint8 (c.a*255.0)
        ()

    member this.Render (shader : float2 -> float4) = 
        let width = int this.imageData.width
        let height = int this.imageData.height
        let size: float2 = new float2(width,height)
        let halfSize = size * 0.5
        for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let pixel = float2(x, y)
            let position = float2(x, height-y) - halfSize
            let color = shader position 
            this.SetPixel this.imageData pixel color

        this.context.putImageData(this.imageData, 0.0, 0.0)

