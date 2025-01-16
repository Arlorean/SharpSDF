module SharpSDF.WebGLRenderer

open Browser.WebGL
open Browser.Types
open Browser

open SharpSDF.HLSL
open Fable.Core

// CanvasRenderingContext3D, string
//export function renderShader( gl, fragmentShaderSource )

[<Import("renderShader", "./GLSLCanvas.js")>]
let renderShader( gl : WebGLRenderingContext, source : string ) : unit  = jsNative

type WebGLRenderer =
    val private canvas: HTMLCanvasElement
    val private context: WebGLRenderingContext

    new (canvasId:string) = 
        let canvas = document.querySelector(canvasId) :?> HTMLCanvasElement
        let context = canvas.getContext("webgl") :?> WebGLRenderingContext
        { canvas=canvas; context=context }

    member this.RenderSource (source : string) = 
        renderShader( this.context, source )

