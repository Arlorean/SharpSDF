module SharpSDF.Colors

open HLSL

let clear    = float4(0,0,0,0) 

let black    = float4(float3(0,0,0),1) 
let blue     = float4(float3(0,0,1),1)
let green    = float4(float3(0,1,0),1)
let cyan     = float4(float3(0,1,1),1)
let red      = float4(float3(1,0,0),1)
let magenta  = float4(float3(1,0,1),1)
let yellow   = float4(float3(1,1,0),1)
let white    = float4(float3(1,1,1),1)

let gray05   = float4(float3(0.05),1) 
let gray10   = float4(float3(0.1),1) 
let gray25   = float4(float3(0.25),1) 
let gray50   = float4(float3(0.5),1) 
let gray75   = float4(float3(0.75),1) 
let gray90   = float4(float3(0.9),1) 
let gray95   = float4(float3(0.95),1) 
