module SharpSDF.WebGLRenderer

open Browser.Types
open Browser

open SharpSDF.HLSL
open Fable.Core

type IWebGLContext = interface end

// CanvasRenderingContext3D, string
//export function renderShader( gl, fragmentShaderSource )

[<Import("renderShader", "./GLSLCanvas.js")>]
let renderShader( gl : IWebGLContext, source : string ) : unit  = jsNative

type WebGLRenderer =
    val private canvas: HTMLCanvasElement
    val private context: IWebGLContext

    new (canvasId:string) = 
        let canvas = document.querySelector(".view") :?> HTMLCanvasElement
        let context = canvas.getContext("webgl") :?> IWebGLContext
        { canvas=canvas; context=context }

    member this.RenderSource (source : string) = 
        renderShader( this.context, source )

