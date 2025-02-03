module SharpSDF.Shaders

open Ast2

type SignedDistance = float
type Color = float4
type Position = float2

type ShapeFn = Position -> SignedDistance   // Native shape function
type ShaderFn = SignedDistance -> Color
type Shader = Color -> SignedDistance -> Color

let pipe (s1:Shader) (s2:Shader) :Shader =
    fun p d ->
        let p2 = s1 p d
        let p3 = s2 p2 d
        p3

let (|>>) = pipe

let solidFill (fillColor:Color) : Shader = 
    fun (background:Color) (sd:SignedDistance) ->
        If (sd <= 0.0) (fillColor,background)

let blend (src:Color) (dst:Color) = 
    Color(dst.rgb*(float3(1.0)-float3(src.a)) + (src.rgb)*float3(src.a), min(src.a+dst.a, 1.0))

let solidStroke (strokeColor:Color) (strokeWidth : float) : Shader =
    fun (background:Color) (sd:SignedDistance) ->
        let hw: float = strokeWidth/2.0
        let d = abs(sd);
        If (d <= hw) (
            //let alpha: float = smoothstep(hw,hw-1.0,d) // 1px anti-alias
            //let stroke = float4(strokeColor.rgb, strokeColor.a * alpha)
            let stroke = strokeColor
            blend stroke background
        ,
            background)

#nowarn 86

//let inline (>=) (v1:float, v2:float) :bool = bool(Bool true)


let shadow (shadowColor:Color) (shadowWidth:float) : Shader =
    fun (background:Color) (sd:SignedDistance) ->
        let alpha :float = invLerp(shadowWidth, 0, sd)
        If (alpha >= 0.0 && alpha <= 1.0) (
            let falloff = alpha*alpha
            let shadow = Color(shadowColor.rgb, shadowColor.a*falloff)
            blend shadow background
        ,
            background)


let outerShadow (shadowColor:Color) (shadowWidth:float) : Shader =
    shadow shadowColor (+shadowWidth)

let innerShadow (shadowColor:Color) (shadowWidth:float) : Shader =
    shadow shadowColor (-shadowWidth)
