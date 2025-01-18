module SharpSDF.SDF

open HLSL
open type Intrinsics

let sdCircle r (p:float2)= length(p) - r

// https://www.shadertoy.com/view/4llXD7
// b.x = (half) width
// b.y = (half) height
// r.x = roundness top-right  
// r.y = roundness bottom-right
// r.z = roundness top-left
// r.w = roundness bottom-left
let sdRoundedBox (hs:float2) (tr,br,tl,bl) (p:float2) =
    let (t,b) = if (p.x > 0.0) then (tr,br) else (tl,bl)
    let r:float = if (p.y > 0.0) then t else b
    let q:float2 = float2(abs(p.x),abs(p.y)) - hs + float2(r,r)
    let a:float = min(max(q.x, q.y), 0.0)
    let b:float = length(max(q, float2(0.0,0.0)))
    a + b - r

let opAddition (d1:float) (d2:float) = min(d1, d2)

let opSubtraction (d1:float) (d2:float) = max(d1, -d2)

let (+++) = opAddition
let (---) = opSubtraction
