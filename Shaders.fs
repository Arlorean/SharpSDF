module SharpSDF.Shaders

open SharpSDF.HLSL

open type HLSL.Intrinsics

type IShader =
    abstract render: distance:float -> float4

type SignedDistance = float
type Color = float4
type Position = float2

type ShapeFn = Position -> SignedDistance   // Native shape function
type ShaderFn = SignedDistance -> Color
type Shader = float4 -> float -> float4

let pipe (s1 : Shader) (s2 : Shader) : Shader =
    fun p d ->
        let p2 = s1 p d
        let p3 = s2 p2 d
        p3

let (|>>) = pipe

let solidFill (fillColor : float4) : Shader = 
    fun (background : float4) (sd : float) ->
        if (sd < 0) then
            fillColor
        else 
            background

let inline invLerp<'T when
    'T: (static member (-):'T * 'T -> 'T) and
    'T: (static member (/):'T * 'T -> 'T)
    > (v1:'T, v2:'T, v:'T) =
    (v - v1) / (v2 - v1)

let blend (src:float4) (dst:float4) = 
    float4(dst.rgb*(1.0-src.a) + src.rgb*(src.a), min(src.a+dst.a, 1.0))

let solidStroke (strokeColor : float4) (strokeWidth : float) : Shader =
    fun (background : float4) (sd : float) ->
        let hw: float = strokeWidth/2.0
        let d = abs(sd);
        if (d <= hw) then
            //let alpha: float = smoothstep(hw,hw-1.0,d) // 1px anti-alias
            //let stroke = float4(strokeColor.rgb, strokeColor.a * alpha)
            let stroke = strokeColor
            blend stroke background
        else
            background

let shadow (shadowColor : float4) (shadowWidth : float) : Shader =
    fun (background : float4) (sd : float) ->
        let alpha = invLerp(shadowWidth, 0, sd)
        if (alpha >= 0.0 && alpha <= 1.0) then
            let falloff = alpha*alpha
            let shadow = float4(shadowColor.rgb, shadowColor.a*falloff)
            blend shadow background
        else 
            background

let outerShadow (shadowColor : float4) (shadowWidth : float) : Shader =
    shadow shadowColor (+shadowWidth)

let innerShadow (shadowColor : float4) (shadowWidth : float) : Shader =
    shadow shadowColor (-shadowWidth)
