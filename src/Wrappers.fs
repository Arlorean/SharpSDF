module SharpSDF.Wrappers

open SharpSDF.Ast

type [<Sealed>] bool =
    val internal expr : Bool
    internal new(expr: Bool) = { expr = expr }
    new(v: System.Boolean) = bool(Bool.Literal v)
    static member internal op_Implicit(v:System.Boolean) = bool(v)
    static member internal op_Implicit(v:bool) = v.expr
    static member If (cond:bool, v1:bool, v2:bool) = bool(Bool.IfThenElse(cond, v1, v2))
    static member (!) (v:bool) = bool(Bool.(!) v)
    static member (&&) (v1:bool, v2:bool) = bool(Bool.(&&)(v1, v2))
    static member (||) (v1:bool, v2:bool) = bool(Bool.(||)(v1, v2))
    static member (==) (v1:bool, v2:bool) = bool(Bool.(==)(v1, v2))
    static member (!=) (v1:bool, v2:bool) = bool(Bool.(!=)(v1, v2))

and [<Sealed>] int =
    val internal expr : Int
    internal new(expr: Int) = { expr = expr }
    new(v: System.Int32) = int(Int.Literal v)
    static member op_Implicit(v:System.Int32) = int(v)
    static member op_Implicit(v:int) = v.expr
    static member If (cond:bool, v1:int, v2:int) = int(Int.IfThenElse(cond, v1, v2))
    static member (~+) (v:int) = int(Int.(~+)(v))
    static member (~-) (v:int) = int(Int.(~-)(v))
    static member (+) (v1:int, v2:int) = int(Int.(+)(v1, v2))
    static member (-) (v1:int, v2:int) = int(Int.(-)(v1, v2))
    static member (*) (v1:int, v2:int) = int(Int.(*)(v1, v2))
    static member (/) (v1:int, v2:int) = int(Int.(/)(v1, v2))
    static member (%) (v1:int, v2:int) = int(Int.(%)(v1, v2))
    static member (==) (v1:int, v2:int) = bool(Bool.CompareInt(EQ, v1, v2))
    static member (!=) (v1:int, v2:int) = bool(Bool.CompareInt(NE, v1, v2))
    static member (<) (v1:int, v2:int) = bool(Bool.CompareInt(LT, v1, v2))
    static member (>) (v1:int, v2:int) = bool(Bool.CompareInt(GT, v1, v2))
    static member (<=) (v1:int, v2:int) = bool(Bool.CompareInt(LE, v1, v2))
    static member (>=) (v1:int, v2:int) = bool(Bool.CompareInt(GE, v1, v2))
    static member abs(v:int) = int(Int.Abs(v) )
    static member clamp(v:int, min:int, max:int) = int(Int.Clamp(v,min,max) )
    static member max(v1:int, v2:int) = int(Int.Max(v1,v2) )
    static member min(v1:int, v2:int) = int(Int.Min(v1,v2) )

and [<Sealed>] float =
    val internal expr : Float
    internal new(expr: Float) = { expr = expr }
    new(v: System.Double) = float(Float.Literal v)
    new(v: System.Int32) = float(Float.Literal v)
    static member op_Implicit(v:System.Double) = float(v)
    static member op_Implicit(v:System.Int32) = float(v)
    static member op_Implicit(v:float) = v.expr
    static member If (cond:bool, v1:float, v2:float) = float(Float.IfThenElse(cond, v1, v2))
    static member (~+) (v:float) = float(Float.(~+)(v))
    static member (~-) (v:float) = float(Float.(~-)(v))
    static member (+) (v1:float, v2:float) = float(Float.(+)(v1, v2))
    static member (-) (v1:float, v2:float) = float(Float.(-)(v1, v2))
    static member (*) (v1:float, v2:float) = float(Float.(*)(v1, v2))
    static member (/) (v1:float, v2:float) = float(Float.(/)(v1, v2))
    static member (%) (v1:float, v2:float) = float(Float.(%)(v1, v2))
    static member (==) (v1:float, v2:float) = bool(Bool.CompareFloat(EQ, v1, v2))
    static member (!=) (v1:float, v2:float) = bool(Bool.CompareFloat(NE, v1, v2))
    static member (<) (v1:float, v2:float) = bool(Bool.CompareFloat(LT, v1, v2))
    static member (>) (v1:float, v2:float) = bool(Bool.CompareFloat(GT, v1, v2))
    static member (<=) (v1:float, v2:float) = bool(Bool.CompareFloat(LE, v1, v2))
    static member (>=) (v1:float, v2:float) = bool(Bool.CompareFloat(GE, v1, v2))
    static member abs(v:float) = float(Float.Abs(v) )
    static member clamp(v:float, min:float, max:float) = float(Float.Clamp(v,min,max) )
    static member exp(v:float) = float(Float.Exp(v) )
    static member lerp(v1:float, v2:float, t:float) = float(Float.Lerp(v1,v2,t) )
    static member max(v1:float, v2:float) = float(Float.Max(v1,v2) )
    static member min(v1:float, v2:float) = float(Float.Min(v1,v2) )
    static member smoothstep(min:float, max:float, v:float) = float(Float.SmoothStep(min,max,v) )
    static member step(v1:float, v2:float) = float(Float.Step(v1,v2) )

and [<Sealed>] bool2 = 
    val x : bool
    val y : bool
    new(x,y) = { x=x; y=y }
    new(v) = bool2(v,v)
    member this.xy = bool2(this.x,this.y)
    member this.yx = bool2(this.y,this.x)
    static member If (cond:bool, v1:bool2, v2:bool2) = bool2(bool.If(cond, v1.x, v2.x), bool.If(cond, v1.y, v2.y))
    static member (!) (v:bool2) = bool2(bool.(!) v.x, bool.(!) v.y)
    static member (&&) (v1:bool2, v2:bool2) = bool2(bool.(&&)(v1.x,v2.x), bool.(&&)(v1.y,v2.y))
    static member (||) (v1:bool2, v2:bool2) = bool2(bool.(||)(v1.x,v2.x), bool.(||)(v1.y,v2.y))
    static member (==) (v1:bool2, v2:bool2) = bool2(bool.(==)(v1.x,v2.x), bool.(==)(v1.y,v2.y))
    static member (!=) (v1:bool2, v2:bool2) = bool2(bool.(!=)(v1.x,v2.x), bool.(!=)(v1.y,v2.y))

and [<Sealed>] bool3 = 
    val x : bool
    val y : bool
    val z : bool
    new(x,y,z) = { x=x; y=y; z=z }
    new(v) = bool3(v,v,v)
    static member If (cond:bool, v1:bool3, v2:bool3) = bool3(bool.If(cond, v1.x, v2.x), bool.If(cond, v1.y, v2.y), bool.If(cond, v1.z, v2.z))
    static member (!) (v:bool3) = bool3(bool.(!) v.x, bool.(!) v.y, bool.(!) v.z)
    static member (&&) (v1:bool3, v2:bool3) = bool3(bool.(&&)(v1.x,v2.x), bool.(&&)(v1.y,v2.y), bool.(&&)(v1.z,v2.z))
    static member (||) (v1:bool3, v2:bool3) = bool3(bool.(||)(v1.x,v2.x), bool.(||)(v1.y,v2.y), bool.(||)(v1.z,v2.z))
    static member (==) (v1:bool3, v2:bool3) = bool3(bool.(==)(v1.x,v2.x), bool.(==)(v1.y,v2.y), bool.(==)(v1.z,v2.z))
    static member (!=) (v1:bool3, v2:bool3) = bool3(bool.(!=)(v1.x,v2.x), bool.(!=)(v1.y,v2.y), bool.(!=)(v1.z,v2.z))

and [<Sealed>] bool4 = 
    val x : bool
    val y : bool
    val z : bool
    val w : bool
    new(x,y,z,w) = { x=x; y=y; z=z; w=w }
    new(v) = bool4(v,v,v,v)
    static member If (cond:bool, v1:bool4, v2:bool4) = bool4(bool.If(cond, v1.x, v2.x), bool.If(cond, v1.y, v2.y), bool.If(cond, v1.z, v2.z), bool.If(cond, v1.w, v2.w))
    static member (!) (v:bool4) = bool4(bool.(!) v.x, bool.(!) v.y, bool.(!) v.z, bool.(!) v.w)
    static member (&&) (v1:bool4, v2:bool4) = bool4(bool.(&&)(v1.x,v2.x), bool.(&&)(v1.y,v2.y), bool.(&&)(v1.z,v2.z), bool.(&&)(v1.w,v2.w))
    static member (||) (v1:bool4, v2:bool4) = bool4(bool.(||)(v1.x,v2.x), bool.(||)(v1.y,v2.y), bool.(||)(v1.z,v2.z), bool.(||)(v1.w,v2.w))
    static member (==) (v1:bool4, v2:bool4) = bool4(bool.(==)(v1.x,v2.x), bool.(==)(v1.y,v2.y), bool.(==)(v1.z,v2.z), bool.(==)(v1.w,v2.w))
    static member (!=) (v1:bool4, v2:bool4) = bool4(bool.(!=)(v1.x,v2.x), bool.(!=)(v1.y,v2.y), bool.(!=)(v1.z,v2.z), bool.(!=)(v1.w,v2.w))

and [<Sealed>] int2 = 
    val x : int
    val y : int
    new(x,y) = { x=x; y=y }
    new(v) = int2(v,v)
    static member If (cond:bool, v1:int2, v2:int2) = int2(int.If(cond, v1.x, v2.x), int.If(cond, v1.y, v2.y))
    static member (+) (v1:int2, v2:int2) = int2(int.(+)(v1.x,v2.x), int.(+)(v1.y,v2.y))
    static member (-) (v1:int2, v2:int2) = int2(int.(-)(v1.x,v2.x), int.(-)(v1.y,v2.y))
    static member (*) (v1:int2, v2:int2) = int2(int.(*)(v1.x,v2.x), int.(*)(v1.y,v2.y))
    static member (/) (v1:int2, v2:int2) = int2(int.(/)(v1.x,v2.x), int.(/)(v1.y,v2.y))
    static member (%) (v1:int2, v2:int2) = int2(int.(%)(v1.x,v2.x), int.(%)(v1.y,v2.y))
    static member (==) (v1:int2, v2:int2) = bool2(int.(==)(v1.x,v2.x), int.(==)(v1.y,v2.y))
    static member (!=) (v1:int2, v2:int2) = bool2(int.(!=)(v1.x,v2.x), int.(!=)(v1.y,v2.y))
    static member abs(v:int2) = int2(int.abs(v.x), int.abs(v.y))
    static member clamp(v:int2, min:int2, max:int2) = int2(int.clamp(v.x,min.x,max.x), int.clamp(v.y,min.y,max.y))
    static member max(v1:int2, v2:int2) = int2(int.max(v1.x,v2.x), int.max(v1.y,v2.y))
    static member min(v1:int2, v2:int2) = int2(int.min(v1.x,v2.x), int.min(v1.y,v2.y))


and [<Sealed>] int3 = 
    val x : int
    val y : int
    val z : int
    new(x,y,z) = { x=x; y=y; z=z }
    new(v) = int3(v,v,v)
    static member If (cond:bool, v1:int3, v2:int3) = int3(int.If(cond, v1.x, v2.x), int.If(cond, v1.y, v2.y), int.If(cond, v1.z, v2.z))
    static member (+) (v1:int3, v2:int3) = int3(int.(+)(v1.x,v2.x), int.(+)(v1.y,v2.y), int.(+)(v1.z,v2.z))
    static member (-) (v1:int3, v2:int3) = int3(int.(-)(v1.x,v2.x), int.(-)(v1.y,v2.y), int.(-)(v1.z,v2.z))
    static member (*) (v1:int3, v2:int3) = int3(int.(*)(v1.x,v2.x), int.(*)(v1.y,v2.y), int.(*)(v1.z,v2.z))
    static member (/) (v1:int3, v2:int3) = int3(int.(/)(v1.x,v2.x), int.(/)(v1.y,v2.y), int.(/)(v1.z,v2.z))
    static member (%) (v1:int3, v2:int3) = int3(int.(%)(v1.x,v2.x), int.(%)(v1.y,v2.y), int.(%)(v1.z,v2.z))
    static member (==) (v1:int3, v2:int3) = bool3(int.(==)(v1.x,v2.x), int.(==)(v1.y,v2.y), int.(==)(v1.z,v2.z))
    static member (!=) (v1:int3, v2:int3) = bool3(int.(!=)(v1.x,v2.x), int.(!=)(v1.y,v2.y), int.(!=)(v1.z,v2.z))

and [<Sealed>] int4 = 
    val x : int
    val y : int
    val z : int
    val w : int
    new(x,y,z,w) = { x=x; y=y; z=z; w=w }
    new(v) = int4(v,v,v,v)
    static member If (cond:bool, v1:int4, v2:int4) = int4(int.If(cond, v1.x, v2.x), int.If(cond, v1.y, v2.y), int.If(cond, v1.z, v2.z), int.If(cond, v1.w, v2.w))
    static member (+) (v1:int4, v2:int4) = int4(int.(+)(v1.x,v2.x), int.(+)(v1.y,v2.y), int.(+)(v1.z,v2.z), int.(+)(v1.w,v2.w))
    static member (-) (v1:int4, v2:int4) = int4(int.(-)(v1.x,v2.x), int.(-)(v1.y,v2.y), int.(-)(v1.z,v2.z), int.(-)(v1.w,v2.w))
    static member (*) (v1:int4, v2:int4) = int4(int.(*)(v1.x,v2.x), int.(*)(v1.y,v2.y), int.(*)(v1.z,v2.z), int.(*)(v1.w,v2.w))
    static member (/) (v1:int4, v2:int4) = int4(int.(/)(v1.x,v2.x), int.(/)(v1.y,v2.y), int.(/)(v1.z,v2.z), int.(/)(v1.w,v2.w))
    static member (%) (v1:int4, v2:int4) = int4(int.(%)(v1.x,v2.x), int.(%)(v1.y,v2.y), int.(%)(v1.z,v2.z), int.(%)(v1.w,v2.w))
    static member (==) (v1:int4, v2:int4) = bool4(int.(==)(v1.x,v2.x), int.(==)(v1.y,v2.y), int.(==)(v1.z,v2.z), int.(==)(v1.w,v2.w))
    static member (!=) (v1:int4, v2:int4) = bool4(int.(!=)(v1.x,v2.x), int.(!=)(v1.y,v2.y), int.(!=)(v1.z,v2.z), int.(!=)(v1.w,v2.w))
    member this.xy = int2(this.x, this.y)
    member this.zw = int2(this.z, this.w)
    member this.r = this.x
    member this.g = this.y
    member this.b = this.z
    member this.a = this.w
    member this.rgb = int3(this.r, this.g, this.b)

and [<Sealed>] float2 = 
    val x : float
    val y : float
    new(x,y) = { x=x; y=y }
    new(v) = float2(v,v)
    static member If (cond:bool, v1:float2, v2:float2) = float2(float.If(cond, v1.x, v2.x), float.If(cond, v1.y, v2.y))
    static member (+) (v1:float2, v2:float2) = float2(float.(+)(v1.x,v2.x), float.(+)(v1.y,v2.y))
    static member (-) (v1:float2, v2:float2) = float2(float.(-)(v1.x,v2.x), float.(-)(v1.y,v2.y))
    static member (*) (v1:float2, v2:float2) = float2(float.(*)(v1.x,v2.x), float.(*)(v1.y,v2.y))
    static member (/) (v1:float2, v2:float2) = float2(float.(/)(v1.x,v2.x), float.(/)(v1.y,v2.y))
    static member (%) (v1:float2, v2:float2) = float2(float.(%)(v1.x,v2.x), float.(%)(v1.y,v2.y))
    static member (==) (v1:float2, v2:float2) = bool2(float.(==)(v1.x,v2.x), float.(==)(v1.y,v2.y))
    static member (!=) (v1:float2, v2:float2) = bool2(float.(!=)(v1.x,v2.x), float.(!=)(v1.y,v2.y))
    static member abs(v:float2) = float2(float.abs(v.x), float.abs(v.y))
    static member clamp(v:float2, min:float2, max:float2) = float2(float.clamp(v.x,min.x,max.x), float.clamp(v.y,min.y,max.y))
    static member exp(v:float2) = float2(float.exp(v.x), float.exp(v.y))
    static member length(v:float2) = float(Float.Length2(v.x,v.y))
    static member lerp(v1:float2, v2:float2, t:float2) = float2(float.lerp(v1.x,v2.x,t.x), float.lerp(v1.y,v2.y,t.y))
    static member max(v1:float2, v2:float2) = float2(float.max(v1.x,v2.x), float.max(v1.y,v2.y))
    static member min(v1:float2, v2:float2) = float2(float.min(v1.x,v2.x), float.min(v1.y,v2.y))
    static member smoothstep(min:float2, max:float2, v:float2) = float2(float.smoothstep(min.x,max.x,v.x), float.smoothstep(min.y,max.y,v.y))
    static member step(v1:float2, v2:float2) = float2(float.step(v1.x,v2.x), float.step(v1.y,v2.y))

and [<Sealed>] float3 = 
    val x : float
    val y : float
    val z : float
    new(x,y,z) = { x=x; y=y; z=z }
    new(v) = float3(v,v,v)
    static member If (cond:bool, v1:float3, v2:float3) = float3(float.If(cond, v1.x, v2.x), float.If(cond, v1.y, v2.y), float.If(cond, v1.z, v2.z))
    static member (+) (v1:float3, v2:float3) = float3(float.(+)(v1.x,v2.x), float.(+)(v1.y,v2.y), float.(+)(v1.z,v2.z))
    static member (-) (v1:float3, v2:float3) = float3(float.(-)(v1.x,v2.x), float.(-)(v1.y,v2.y), float.(-)(v1.z,v2.z))
    static member (*) (v1:float3, v2:float3) = float3(float.(*)(v1.x,v2.x), float.(*)(v1.y,v2.y), float.(*)(v1.z,v2.z))
    static member (/) (v1:float3, v2:float3) = float3(float.(/)(v1.x,v2.x), float.(/)(v1.y,v2.y), float.(/)(v1.z,v2.z))
    static member (%) (v1:float3, v2:float3) = float3(float.(%)(v1.x,v2.x), float.(%)(v1.y,v2.y), float.(%)(v1.z,v2.z))
    static member (==) (v1:float3, v2:float3) = bool3(float.(==)(v1.x,v2.x), float.(==)(v1.y,v2.y), float.(==)(v1.z,v2.z))
    static member (!=) (v1:float3, v2:float3) = bool3(float.(!=)(v1.x,v2.x), float.(!=)(v1.y,v2.y), float.(!=)(v1.z,v2.z))
    static member abs(v:float3) = float3(float.abs(v.x), float.abs(v.y), float.abs(v.z))
    static member clamp(v:float3, min:float3, max:float3) = float3(float.clamp(v.x,min.x,max.x), float.clamp(v.y,min.y,max.y), float.clamp(v.z,min.z,max.z))
    static member exp(v:float3) = float3(float.exp(v.x), float.exp(v.y), float.exp(v.z))
    static member length(v:float3) = float(Float.Length3(v.x,v.y,v.z))
    static member lerp(v1:float3, v2:float3, t:float3) = float3(float.lerp(v1.x,v2.x,t.x), float.lerp(v1.y,v2.y,t.y), float.lerp(v1.z,v2.z,t.z))
    static member max(v1:float3, v2:float3) = float3(float.max(v1.x,v2.x), float.max(v1.y,v2.y), float.max(v1.z,v2.z))
    static member min(v1:float3, v2:float3) = float3(float.min(v1.x,v2.x), float.min(v1.y,v2.y), float.min(v1.z,v2.z))
    static member smoothstep(min:float3, max:float3, v:float3) = float3(float.smoothstep(min.x,max.x,v.x), float.smoothstep(min.y,max.y,v.y), float.smoothstep(min.z,max.z,v.z))
    static member step(v1:float3, v2:float3) = float3(float.step(v1.x,v2.x), float.step(v1.y,v2.y), float.step(v1.z,v2.z))

and [<Sealed>] float4 = 
    val x : float
    val y : float
    val z : float
    val w : float
    new(x,y,z,w) = { x=x; y=y; z=z; w=w }
    new(v:float3,w) = { x=v.x; y=v.y; z=v.z; w=w }
    new(v) = float4(v,v,v,v)
    static member If (cond:bool, v1:float4, v2:float4) = float4(float.If(cond, v1.x, v2.x), float.If(cond, v1.y, v2.y), float.If(cond, v1.z, v2.z), float.If(cond, v1.w, v2.w))
    static member (+) (v1:float4, v2:float4) = float4(float.(+)(v1.x,v2.x), float.(+)(v1.y,v2.y), float.(+)(v1.z,v2.z), float.(+)(v1.w,v2.w))
    static member (-) (v1:float4, v2:float4) = float4(float.(-)(v1.x,v2.x), float.(-)(v1.y,v2.y), float.(-)(v1.z,v2.z), float.(-)(v1.w,v2.w))
    static member (*) (v1:float4, v2:float4) = float4(float.(*)(v1.x,v2.x), float.(*)(v1.y,v2.y), float.(*)(v1.z,v2.z), float.(*)(v1.w,v2.w))
    static member (/) (v1:float4, v2:float4) = float4(float.(/)(v1.x,v2.x), float.(/)(v1.y,v2.y), float.(/)(v1.z,v2.z), float.(/)(v1.w,v2.w))
    static member (==) (v1:float4, v2:float4) = bool4(float.(==)(v1.x,v2.x), float.(==)(v1.y,v2.y), float.(==)(v1.z,v2.z), float.(==)(v1.w,v2.w))
    static member (!=) (v1:float4, v2:float4) = bool4(float.(!=)(v1.x,v2.x), float.(!=)(v1.y,v2.y), float.(!=)(v1.z,v2.z), float.(!=)(v1.w,v2.w))
    member this.xy = float2(this.x, this.y)
    member this.zw = float2(this.z, this.w)
    member this.r = this.x
    member this.g = this.y
    member this.b = this.z
    member this.a = this.w
    member this.rgb = float3(this.r, this.g, this.b)
    static member abs(v:float4) = float4(float.abs(v.x), float.abs(v.y), float.abs(v.z), float.abs(v.w))
    static member clamp(v:float4, min:float4, max:float4) = float4(float.clamp(v.x,min.x,max.x), float.clamp(v.y,min.y,max.y), float.clamp(v.z,min.z,max.z), float.clamp(v.w,min.w,max.w))
    static member exp(v:float4) = float4(float.exp(v.x), float.exp(v.y), float.exp(v.z), float.exp(v.w))
    static member length(v:float4) = float(Float.Length4(v.x,v.y,v.z,v.w))
    static member lerp(v1:float4, v2:float4, t:float4) = float4(float.lerp(v1.x,v2.x,t.x), float.lerp(v1.y,v2.y,t.y), float.lerp(v1.z,v2.z,t.z), float.lerp(v1.w,v2.w,t.w))
    static member max(v1:float4, v2:float4) = float4(float.max(v1.x,v2.x), float.max(v1.y,v2.y), float.max(v1.z,v2.z), float.max(v1.w,v2.w))
    static member min(v1:float4, v2:float4) = float4(float.min(v1.x,v2.x), float.min(v1.y,v2.y), float.min(v1.z,v2.z), float.min(v1.w,v2.w))
    static member smoothstep(min:float4, max:float4, v:float4) = float4(float.smoothstep(min.x,max.x,v.x), float.smoothstep(min.y,max.y,v.y), float.smoothstep(min.z,max.z,v.z), float.smoothstep(min.w,max.w,v.w))
    static member step(v1:float4, v2:float4) = float4(float.step(v1.x,v2.x), float.step(v1.y,v2.y), float.step(v1.z,v2.z), float.step(v1.w,v2.w))


let inline abs (v:'T):'T when 'T:(static member abs : 'T -> 'T) = 'T.abs(v)
let inline clamp (v:'T, min:'T, max:'T):'T when 'T:(static member clamp : 'T * 'T * 'T -> 'T) = 'T.clamp(v, min, max)
let inline exp (v:'T):'T when 'T:(static member exp : 'T -> 'T) = 'T.exp(v)
let inline length (v:'T):float when 'T:(static member length : 'T -> float)= 'T.length(v)
let inline lerp (v1:'T, v2:'T, t:'T):'T when 'T:(static member lerp : 'T * 'T * 'T -> 'T) = 'T.lerp(v1, v2, t)
let inline max (v1:'T, v2:'T):'T when 'T:(static member max : 'T * 'T -> 'T) = 'T.max(v1, v2)
let inline min (v1:'T, v2:'T):'T when 'T:(static member min : 'T * 'T -> 'T) = 'T.min(v1, v2)
let inline smoothstep (min:'T, max:'T, v:'T):'T when 'T:(static member smoothstep : 'T * 'T * 'T -> 'T) = 'T.smoothstep(min, max, v)
let inline step (v1:'T, v2:'T):'T when 'T:(static member step : 'T * 'T -> 'T) = 'T.step(v1, v2)

let inline (<) (v1:'T) (v2:'T) : 'U when 'T:(static member (<) : 'T * 'T -> 'U)= 'T.(<) (v1,v2)
let inline (>) (v1:'T) (v2:'T) : 'U when 'T:(static member (>) : 'T * 'T -> 'U)= 'T.(>) (v1,v2)
let inline (<=) (v1:'T) (v2:'T) : 'U when 'T:(static member (<=) : 'T * 'T -> 'U)= 'T.(<=) (v1,v2)
let inline (>=) (v1:'T) (v2:'T) : 'U when 'T:(static member (>=) : 'T * 'T -> 'U)= 'T.(>=) (v1,v2)
let inline (*) (v1:'T) (v2:'T) : 'T when 'T:(static member (*) : 'T * 'T -> 'T)= 'T.(*) (v1,v2)
let inline (/) (v1:'T) (v2:'T) : 'T when 'T:(static member (/) : 'T * 'T -> 'T)= 'T.(/) (v1,v2)
let inline (&&) (v1:'T) (v2:'T) : 'T when 'T:(static member (&&) : 'T * 'T -> 'T) = 'T.(&&) (v1,v2)
let inline (||) (v1:'T) (v2:'T) : 'T when 'T:(static member (||) : 'T * 'T -> 'T) = 'T.(||) (v1,v2)

let inline If (cond:bool) (v1:'T,v2:'T) : 'T when 'T:(static member If : bool * 'T * 'T -> 'T) ='T.If (cond,v1,v2)

let inline invLerp (v1:'T, v2:'T, v:'T) :'T =
    (v - v1) / (v2 - v1)


// Compile Tests
let foo (a:float2) = float3(a.x,a.y,0)

let v1 = float4(1,2,3,4)
let v2 = float4(1,2,3,5)
let a = (v1 == v2)
let b = (v1 != v2)
let c = (v1 + v2)
let d = (v1 - v2)
let l = length v1

let f1 = float(1.0)
let f2 = float(2.0)
let f3 = (f1==f2)

