module SharpSDF.Shaders

open SharpSDF.HLSL
open SharpSDF.SDF

open type SharpSDF.HLSL.Intrinsics

type IShader =
    abstract render: distance:float -> float4

/// Context allows us to implement shapes that transform the position (eg. translation)
type Position = 
    {
        Xy : float2
    }
    with
        static member Create( p ) = { Xy = p }
        member __.X = __.Xy.x
        member __.Y = __.Xy.y
        member __.Offset( x, y ) = { __ with Xy = float2( __.X + x, __.Y  + y ) }
        member __.Offset( xy : float2 ) = { __ with Xy = float2( __.X + xy.x, __.Y  + xy.y ) }

type SignedDistance = float
type Color = float4

type ShapeFn = Position -> SignedDistance
type ShaderFn = SignedDistance -> Color

/// A wrapper for a native shape function. Allows us to attach metadata to a native 
/// shape function (eg name, source location, arguments, etc)
type SdfShape = 
    {
        Sdf : ShapeFn
        Name : string
    }
    override __.ToString (): string = __.Name

/// Calculate signed distance field from a context and shape
let sdf (shape : SdfShape) (ctx : Position) : float =
    ctx |> shape.Sdf 

/// Shape whose function gets access to the Position object
let mkShape name fn = { Sdf = fn; Name = name }

let mkShapeBasic name (fn : float2 -> float) 
    = { Sdf = (fun p -> fn p.Xy) ; Name = name }

//let mkShapeWithMeta name fn = { Sdf = fn; Name = name }

let _circle (r : float) : SdfShape =
    sdCircle r |> mkShapeBasic (sprintf "circle %f" r)

let _translate (offset : float2) (s1 : SdfShape) : SdfShape =
    fun (ctx : Position) ->
        ctx.Offset(offset) |> sdf s1
    |> mkShape(sprintf "translate [%s] by (%f,%f)" s1.Name (offset.x) (offset.y))

let _add (s1 : SdfShape) (s2 : SdfShape) : SdfShape =
    s1

let _sub (s1 : SdfShape) (s2 : SdfShape) : SdfShape =
    fun ctx ->
        opSubtraction 
            (sdf s1 ctx) 
            (sdf s2 ctx)
    |> mkShape(sprintf "%s-%s" s1.Name s2.Name)

let (+.) = _add

let (-.) = _sub

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
