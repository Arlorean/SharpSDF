module SharpSDF.ShaderGenerator

open HLSL

let makeShader ( shader : (float2 -> float4) ) : string = """
    void main() {
        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); // Red color
    }
    """
