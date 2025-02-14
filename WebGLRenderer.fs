module SharpSDF.WebGLRenderer

open Browser.Types
open Browser

open Fable.Core

[<Import("renderShader", "./GLSLCanvas.js")>]
let renderShader( gl:WebGLRenderingContext, source:string, width:float, height:float ) : unit  = jsNative

type WebGLRenderer =
    val private canvas: HTMLCanvasElement
    val private context: WebGLRenderingContext

    new (canvasId:string) = 
        let canvas = document.querySelector(canvasId) :?> HTMLCanvasElement
        let context = canvas.getContext("webgl2") :?> WebGLRenderingContext
        { canvas=canvas; context=context }

    member this.RenderSource (source : string) = 
        renderShader( this.context, source, this.canvas.width, this.canvas.height )

