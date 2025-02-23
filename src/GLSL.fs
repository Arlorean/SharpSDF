module SharpSDF.GLSL

open SharpSDF.Ast

let floatString (v:float) = 
    match string v with
    | s when s.Contains('.') -> s
    | s -> s+".0"

let compare = function
    | EQ -> "=="
    | NE -> "!="
    | LT -> "<"
    | GT -> ">"
    | LE -> "<="
    | GE -> ">="

let rec EvalB(v:Bool) : string =
    let Eval = EvalB
    match v with
    | Bool.Varying s -> s
    | Bool.Literal v -> string v
    | Bool.IfThenElse (cond,t,f) -> $"({EvalB cond} ? ({Eval t}) : ({Eval f}))"
    | Bool.(!) v -> $"!({Eval v})"
    | Bool.(&&) (v1, v2) -> $"({Eval v1} && {Eval v2})"
    | Bool.(||) (v1, v2) -> $"({Eval v1} || {Eval v2})"
    | Bool.(==) (v1, v2) -> $"({Eval v1} == {Eval v2})"
    | Bool.(!=) (v1, v2) -> $"({Eval v1} != {Eval v2})"
    | Bool.CompareInt (fn, v1, v2) -> $"({EvalI v1} {compare fn} {EvalI v2})"
    | Bool.CompareFloat (fn, v1, v2) -> $"({EvalF v1} {compare fn} {EvalF v2})"

and EvalI(v:Int) : string =
    let Eval = EvalI
    match v with
    | Int.Varying s -> s
    | Int.Literal v -> string v
    | Int.IfThenElse (cond,t,f) -> $"({EvalB cond} ? ({Eval t}) : ({Eval f}))"
    | Int.(~-) v -> $"-({Eval v})"
    | Int.(~+) v -> $"+({Eval v})"
    | Int.(+) (v1, v2) -> $"({Eval v1} + {Eval v2})"
    | Int.(-) (v1, v2) -> $"({Eval v1} - {Eval v2})"
    | Int.(*) (v1, v2) -> $"({Eval v1} * {Eval v2})"
    | Int.(/) (v1, v2) -> $"({Eval v1} / {Eval v2})"
    | Int.(%) (v1, v2) -> $"({Eval v1} %% {Eval v2})"
    | Int.Abs v -> $"abs({Eval v})"
    | Int.Clamp (v, min, max) -> $"clamp({Eval v}, {Eval min}, {Eval max})"
    | Int.Max (v1, v2) -> $"max({Eval v1}, {Eval v2})"
    | Int.Min (v1, v2) -> $"min({Eval v1}, {Eval v2})"

and EvalF(v:Float) : string =
    let Eval = EvalF
    match v with
    | Float.Varying s -> s
    | Float.Literal v -> floatString v
    | Float.Length2 (x, y) -> $"length(vec2({Eval x}, {Eval y}))"
    | Float.Length3 (x, y, z) -> $"length(vec3({Eval x}, {Eval y}, {Eval z}))"
    | Float.Length4 (x, y, z, w) -> $"length(vec4({Eval x}, {Eval y}, {Eval z}, {Eval w}))"
    | Float.IfThenElse (cond,t,f) -> $"({EvalB cond} ? ({Eval t}) : ({Eval f}))"
    | Float.(~-) v -> $"-({Eval v})"
    | Float.(~+) v -> $"+({Eval v})"
    | Float.(+) (v1, v2) -> $"({Eval v1} + {Eval v2})"
    | Float.(-) (v1, v2) -> $"({Eval v1} - {Eval v2})"
    | Float.(*) (v1, v2) -> $"({Eval v1} * {Eval v2})"
    | Float.(/) (v1, v2) -> $"({Eval v1} / {Eval v2})"
    | Float.(%) (v1, v2) -> $"({Eval v1} %% {Eval v2}()"
    | Float.Abs v -> $"abs({Eval v})"
    | Float.Clamp (v, min, max) -> $"clamp({Eval v}, {Eval min}, {Eval max})"
    | Float.Exp v -> $"exp({Eval v})"
    | Float.Lerp (v1, v2, t) -> $"lerp({Eval v1}, {Eval v2}, {Eval t})"
    | Float.Max (v1, v2) -> $"max({Eval v1}, {Eval v2})"
    | Float.Min (v1, v2) -> $"min({Eval v1}, {Eval v2})"
    | Float.SmoothStep (min, max, v) -> $"smoothstep({Eval min}, {Eval max}, {Eval v})"
    | Float.Step (v1, v2) -> $"step({Eval v1}, {Eval v2})"

let GenerateMainImage (shader : Wrappers.float2 -> Wrappers.float4) = 
    let color = Wrappers.float2(Wrappers.float(Float.Varying "p.x"), Wrappers.float(Float.Varying "p.y")) |> shader
    $"""
void mainImage( out vec4 fragColor, in vec2 fragCoord ) {{
    vec2 p = fragCoord - iResolution.xy*0.5;
    fragColor = vec4(
        {EvalF color.r.expr},
        {EvalF color.g.expr},
        {EvalF color.b.expr},
        {EvalF color.a.expr}
    );
}}
    """

let GenerateFragment (shader : Wrappers.float2 -> Wrappers.float4) = 
    $"""#version 300 es

precision mediump float;

// Standard ShaderToy Uniforms
// https://www.shadertoy.com/howto

uniform vec3 iResolution;
uniform float iTime;
uniform float iTimeDelta;
uniform float iFrame;
uniform float iChannelTime[4];
uniform vec4 iMouse;
uniform vec4 iDate;
uniform float iSampleRate;
uniform vec3 iChannelResolution[4];

out vec4 glFragColor;

{GenerateMainImage shader}

void main()  {{
    glFragColor.w = 1.;

    mainImage(glFragColor, gl_FragCoord.xy);
}}
    """