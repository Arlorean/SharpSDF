module SharpSDF.HLSL

open System

type float = System.Double

[<Struct>]
type float2 =    
    val mutable x : float
    val mutable y : float

    new (x:float, y:float) = { x=x; y=y }
    new (x:float) = float2(x, x)

    new (x:int, y:int) = { x=x; y=y }

    static member (*) (v:float2, t:float) = float2(v.x*t, v.y*t)

    static member (+) (v1:float2, v2:float2) = float2(v1.x+v2.x, v1.y+v2.y)
    static member (-) (v1:float2, v2:float2) = float2(v1.x-v2.x, v1.y-v2.y)
    static member (*) (v1:float2, v2:float2) = float2(v1.x*v2.x, v1.y*v2.y)
    static member (/) (v1:float2, v2:float2) = float2(v1.x/v2.x, v1.y/v2.y)

[<Struct>]
type float3 =
    val mutable x : float
    val mutable y : float
    val mutable z : float

    new (x:float, y:float, z:float) = { x=x; y=y; z=z }
    new (x:float, y:float) = float3(x, y, 0.0)
    new (v:float) = float3(v, v, v)

    static member (*) (v:float3, t:float) = float3(v.x*t, v.y*t, v.z*t)

    static member (+) (v1:float3, v2:float3) = float3(v1.x+v2.x, v1.y+v2.y, v1.z+v2.z)
    static member (-) (v1:float3, v2:float3) = float3(v1.x-v2.x, v1.y-v2.y, v1.z-v2.z)
    static member (*) (v1:float3, v2:float3) = float3(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z)
    static member (/) (v1:float3, v2:float3) = float3(v1.x/v2.x, v1.y/v2.y, v1.z/v2.z)


[<Struct>]
type float4 =
    val mutable x : float
    val mutable y : float
    val mutable z : float
    val mutable w : float

    new (x:float, y:float, z:float, w:float) = { x=x; y=y; z=z; w=w }
    new (v:float3, w:float) = { x=v.x; y=v.y; z=v.z; w=w }
    new (x:float, y:float, z:float) = float4(x, y, z, 0.0)
    new (x:float, y:float) = float4(x, y, 0.0, 0.0)
    new (v:float) = float4(v, v, v, v)

    member this.r with get() = this.x and set(v:float) = this.x <- v
    member this.g with get() = this.y and set(v:float) = this.y <- v
    member this.b with get() = this.z and set(v:float) = this.z <- v
    member this.a with get() = this.w and set(v:float) = this.w <- v

    member this.xyz with get() = float3(this.x, this.y, this.z)
    member this.rgb with get() = this.xyz


    static member (*) (v:float4, t:float) = float4(v.x*t, v.y*t, v.z*t, v.w*t)

    static member (+) (v1:float4, v2:float4) = float4(v1.x+v2.x, v1.y+v2.y, v1.z+v2.z, v1.w+v2.w)
    static member (-) (v1:float4, v2:float4) = float4(v1.x-v2.x, v1.y-v2.y, v1.z-v2.z, v1.w-v2.w)
    static member (*) (v1:float4, v2:float4) = float4(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z, v1.w*v2.w)
    static member (/) (v1:float4, v2:float4) = float4(v1.x/v2.x, v1.y/v2.y, v1.z/v2.z, v1.w/v2.w)


type private F1 =
    static member internal Abs(v:float) = Math.Abs(v)
    static member internal Clamp(v:float, min:float, max:float) = Math.Clamp(v,min,max)
    static member internal Exp(v:float) = Math.Exp(v)
    static member internal Length(v:float) = v
    static member internal Lerp(v1:float, v2:float, t:float) = v1 + t*(v2-v1)
    static member internal Min(v1:float, v2:float) = Math.Min(v1, v2)
    static member internal Max(v1:float, v2:float) = Math.Max(v1, v2)
    static member internal Smoothstep(min:float, max:float, v:float) =
        let t = F1.Clamp((v - min) / (max - min), 0.0, 1.0)
        t*t*(3.0-2.0*t)
    static member internal Step(v1:float, v2:float) = if v1 >= v2 then 1.0 else 0.0


type private F2 =
    static member internal Abs(v:float2) = float2(
        F1.Abs(v.x),
        F1.Abs(v.y))
    static member internal Exp(v:float2) = float2(
        F1.Exp(v.x),
        F1.Exp(v.y))
    static member internal Clamp(v:float2, min:float2, max:float2) = float2(
        F1.Clamp(v.x, min.x, max.x),
        F1.Clamp(v.y, min.y, max.y))
    static member internal Length(v:float2) = Math.Sqrt(v.x*v.x + v.y*v.y)
    static member internal Lerp(v1:float2, v2:float2, t:float) = float2(
        F1.Lerp(v1.x, v2.x, t),
        F1.Lerp(v1.y, v2.y, t))
    static member internal Min(v1:float2, v2:float2) = float2(
        F1.Min(v1.x, v2.x),
        F1.Min(v1.y, v2.y))
    static member internal Max(v1:float2, v2:float2) = float2(
        F1.Max(v1.x, v2.x),
        F1.Max(v1.y, v2.y))
    static member internal Smoothstep(min:float2, max:float2, v:float2) = float2(
        F1.Smoothstep(min.x, max.x, v.x),
        F1.Smoothstep(min.y, max.y, v.y))
    static member internal Step(v1:float2, v2:float2) = float2(
        F1.Step(v1.x, v2.x),
        F1.Step(v1.y, v2.y))


type private F3 =
    static member internal Abs(v:float3) = float3(
        F1.Abs(v.x),
        F1.Abs(v.y),
        F1.Abs(v.z))
    static member internal Exp(v:float3) = float3(
        F1.Exp(v.x),
        F1.Exp(v.y),
        F1.Exp(v.z))
    static member internal Clamp(v:float3, min:float3, max:float3) = float3(
        F1.Clamp(v.x, min.x, max.x),
        F1.Clamp(v.y, min.y, max.y),
        F1.Clamp(v.z, min.z, max.z))
    static member internal Length(v:float3) = Math.Sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
    static member internal Lerp(v1:float3, v2:float3, t:float) = float3(
        F1.Lerp(v1.x, v2.x, t),
        F1.Lerp(v1.y, v2.y, t),
        F1.Lerp(v1.z, v2.z, t))
    static member internal Min(v1:float3, v2:float3) = float3(
        F1.Min(v1.x, v2.x),
        F1.Min(v1.y, v2.y),
        F1.Min(v1.z, v2.z))
    static member internal Max(v1:float3, v2:float3) = float3(
        F1.Max(v1.x, v2.x),
        F1.Max(v1.y, v2.y),
        F1.Max(v1.z, v2.z))
    static member internal Smoothstep(min:float3, max:float3, v:float3) = float3(
        F1.Smoothstep(min.x, max.x, v.x),
        F1.Smoothstep(min.y, max.y, v.y),
        F1.Smoothstep(min.z, max.z, v.z))
    static member internal Step(v1:float3, v2:float3) = float3(
        F1.Step(v1.x, v2.x),
        F1.Step(v1.y, v2.y),
        F1.Step(v1.z, v2.z))

type private F4 =
    static member internal Abs(v:float4) = float4(
        F1.Abs(v.x),
        F1.Abs(v.y),
        F1.Abs(v.z),
        F1.Abs(v.w))
    static member internal Exp(v:float4) = float4(
        F1.Exp(v.x),
        F1.Exp(v.y),
        F1.Exp(v.z),
        F1.Exp(v.w))
    static member internal Clamp(v:float4, min:float4, max:float4) = float4(
        F1.Clamp(v.x, min.x, max.x),
        F1.Clamp(v.y, min.y, max.y),
        F1.Clamp(v.z, min.z, max.z),
        F1.Clamp(v.w, min.w, max.w))
    static member internal Length(v:float4) = Math.Sqrt(v.x*v.x + v.y*v.y)
    static member internal Lerp(v1:float4, v2:float4, t:float) = float4(
        F1.Lerp(v1.x, v2.x, t),
        F1.Lerp(v1.y, v2.y, t),
        F1.Lerp(v1.z, v2.z, t),
        F1.Lerp(v1.w, v2.w, t))
    static member internal Min(v1:float4, v2:float4) = float4(
        F1.Min(v1.x, v2.x),
        F1.Min(v1.y, v2.y),
        F1.Min(v1.z, v2.z),
        F1.Min(v1.w, v2.w))
    static member internal Max(v1:float4, v2:float4) = float4(
        F1.Max(v1.x, v2.x),
        F1.Max(v1.y, v2.y),
        F1.Max(v1.z, v2.z),
        F1.Max(v1.w, v2.w))
    static member internal Smoothstep(min:float4, max:float4, v:float4) = float4(
        F1.Smoothstep(min.x, max.x, v.x),
        F1.Smoothstep(min.y, max.y, v.y),
        F1.Smoothstep(min.z, max.z, v.z),
        F1.Smoothstep(min.w, max.w, v.w))
    static member internal Step(v1:float4, v2:float4) = float4(
        F1.Step(v1.x, v2.x),
        F1.Step(v1.y, v2.y),
        F1.Step(v1.z, v2.z),
        F1.Step(v1.w, v2.w))

type Intrinsics =
    static member abs (v) = F1.Abs(v)
    static member abs (v) = F2.Abs(v)
    static member abs (v) = F3.Abs(v)
    static member abs (v) = F4.Abs(v)

    static member clamp (v, min, max) = F1.Clamp(v, min, max)
    static member clamp (v, min, max) = F2.Clamp(v, min, max)
    static member clamp (v, min, max) = F3.Clamp(v, min, max)
    static member clamp (v, min, max) = F4.Clamp(v, min, max)

    static member exp (v) = F1.Exp(v)
    static member exp (v) = F2.Exp(v)
    static member exp (v) = F3.Exp(v)
    static member exp (v) = F4.Exp(v)

    static member lerp (v1, v2, t) = F1.Lerp(v1, v2, t)
    static member lerp (v1, v2, t) = F2.Lerp(v1, v2, t)
    static member lerp (v1, v2, t) = F3.Lerp(v1, v2, t)
    static member lerp (v1, v2, t) = F4.Lerp(v1, v2, t)

    static member min (v1, v2) = F1.Min(v1, v2)
    static member min (v1, v2) = F2.Min(v1, v2)
    static member min (v1, v2) = F3.Min(v1, v2)
    static member min (v1, v2) = F4.Min(v1, v2)

    static member max (v1, v2) = F1.Max(v1, v2)
    static member max (v1, v2) = F2.Max(v1, v2)
    static member max (v1, v2) = F3.Max(v1, v2)
    static member max (v1, v2) = F4.Max(v1, v2)

    static member length (v) = F1.Length(v)
    static member length (v) = F2.Length(v)
    static member length (v) = F3.Length(v)
    static member length (v) = F4.Length(v)

    static member smoothstep(min, max, v) = F1.Smoothstep(min,max,v)
    static member smoothstep(min, max, v) = F2.Smoothstep(min,max,v)
    static member smoothstep(min, max, v) = F3.Smoothstep(min,max,v)
    static member smoothstep(min, max, v) = F4.Smoothstep(min,max,v)

    static member step (v1, v2) = F1.Step(v1, v2)
    static member step (v1, v2) = F2.Step(v1, v2)
    static member step (v1, v2) = F3.Step(v1, v2)
    static member step (v1, v2) = F4.Step(v1, v2)

