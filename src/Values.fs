namespace SharpSDF.Values

type bool = System.Boolean
type int = System.Int32
type float = System.Double

[<Struct>]
type float2 =    
    val x : float
    val y : float

    new (x:float, y:float) = { x=x; y=y }
    new (x:float) = float2(x, x)

    new (x:int, y:int) = { x=x; y=y }

    static member (*) (v:float2, t:float) = float2(v.x*t, v.y*t)
    static member (~+) (v:float2) = float2(+v.x, +v.y)
    static member (~-) (v:float2) = float2(-v.x, -v.y)
    static member (+) (v1:float2, v2:float2) = float2(v1.x+v2.x, v1.y+v2.y)
    static member (-) (v1:float2, v2:float2) = float2(v1.x-v2.x, v1.y-v2.y)
    static member (*) (v1:float2, v2:float2) = float2(v1.x*v2.x, v1.y*v2.y)
    static member (/) (v1:float2, v2:float2) = float2(v1.x/v2.x, v1.y/v2.y)
    static member (%) (v1:float2, v2:float2) = float2(v1.x%v2.x, v1.y%v2.y)

[<Struct>]
type float3 =
    val x : float
    val y : float
    val z : float
    new (x:float, y:float, z:float) = { x=x; y=y; z=z }
    new (x:float, y:float) = float3(x, y, 0.0)
    new (v:float) = float3(v, v, v)
    static member (*) (v:float3, t:float) = float3(v.x*t, v.y*t, v.z*t)
    static member (~+) (v:float3) = float3(+v.x, +v.y, +v.z)
    static member (~-) (v:float3) = float3(-v.x, -v.y, -v.z)
    static member (+) (v1:float3, v2:float3) = float3(v1.x+v2.x, v1.y+v2.y, v1.z+v2.z)
    static member (-) (v1:float3, v2:float3) = float3(v1.x-v2.x, v1.y-v2.y, v1.z-v2.z)
    static member (*) (v1:float3, v2:float3) = float3(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z)
    static member (/) (v1:float3, v2:float3) = float3(v1.x/v2.x, v1.y/v2.y, v1.z/v2.z)
    static member (%) (v1:float3, v2:float3) = float3(v1.x%v2.x, v1.y%v2.y, v1.z%v2.z)

[<Struct>]
type float4 =
    val x : float
    val y : float
    val z : float
    val w : float
    new (x:float, y:float, z:float, w:float) = { x=x; y=y; z=z; w=w }
    new (v:float3, w:float) = { x=v.x; y=v.y; z=v.z; w=w }
    new (x:float, y:float, z:float) = float4(x, y, z, 0.0)
    new (x:float, y:float) = float4(x, y, 0.0, 0.0)
    new (v:float) = float4(v, v, v, v)
    member this.r = this.x
    member this.g = this.y
    member this.b = this.z
    member this.a = this.w
    member this.xyz with get() = float3(this.x, this.y, this.z)
    member this.rgb with get() = this.xyz
    static member (*) (v:float4, t:float) = float4(v.x*t, v.y*t, v.z*t, v.w*t)
    static member (~+) (v:float4) = float4(+v.x, +v.y, +v.z, +v.w)
    static member (~-) (v:float4) = float4(-v.x, -v.y, -v.z, -v.w)
    static member (+) (v1:float4, v2:float4) = float4(v1.x+v2.x, v1.y+v2.y, v1.z+v2.z, v1.w+v2.w)
    static member (-) (v1:float4, v2:float4) = float4(v1.x-v2.x, v1.y-v2.y, v1.z-v2.z, v1.w-v2.w)
    static member (*) (v1:float4, v2:float4) = float4(v1.x*v2.x, v1.y*v2.y, v1.z*v2.z, v1.w*v2.w)
    static member (/) (v1:float4, v2:float4) = float4(v1.x/v2.x, v1.y/v2.y, v1.z/v2.z, v1.w/v2.w)
    static member (%) (v1:float4, v2:float4) = float4(v1.x%v2.x, v1.y%v2.y, v1.z%v2.z, v1.w%v2.w)

[<Struct>]
type bool2 =    
    val x : bool
    val y : bool
    new (x, y) = { x=x; y=y }

[<Struct>]
type bool3 =    
    val x : bool
    val y : bool
    val z : bool
    new (x, y, z) = { x=x; y=y; z=z }

[<Struct>]
type bool4 =    
    val x : bool
    val y : bool
    val z : bool
    val w : bool
    new (x, y, z, w) = { x=x; y=y; z=z; w=w }

[<Struct>]
type int2 =    
    val x : int
    val y : int
    new (x, y) = { x=x; y=y }

[<Struct>]
type int3 =    
    val x : int
    val y : int
    val z : int
    new (x, y, z) = { x=x; y=y; z=z }

[<Struct>]
type int4 =    
    val x : int
    val y : int
    val z : int
    val w : int
    new (x, y, z, w) = { x=x; y=y; z=z; w=w }