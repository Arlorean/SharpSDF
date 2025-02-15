module SharpSDF.ShaderToy

open SharpSDF.HLSL
open type HLSL.Intrinsics

let innerColor = new float4(0.65,0.85,1.0)
let outerColor = new float4(0.9,0.6,0.3)

let Render distance =
    let d = distance/200.0;
    let baseColor = if (d<=0.0) then innerColor else outerColor
    let contourColor =
        baseColor
            * (1.0 - exp(-6.0*abs(d)))
            * (0.8 + 0.2*cos(150.0*d))
    let c : float4 = lerp( contourColor, float4(1,1,1), 1.0-smoothstep(0.0,0.01,abs(d)) );
    float4(c.r,c.g,c.b,1.0)
