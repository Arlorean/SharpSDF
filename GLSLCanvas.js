// Vertex Shader Source
const vertexShaderSource = `#version 300 es
    in vec4 aPosition;
    void main() {
        gl_Position = aPosition;
    }
`;

// Compile Shader
function compileShader(gl, source, type) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error("Error compiling shader:", gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    return shader;
}

// Create Program
function createProgram(gl, vertexSource, fragmentSource) {
    const vertexShader = compileShader(gl, vertexSource, gl.VERTEX_SHADER);
    const fragmentShader = compileShader(gl, fragmentSource, gl.FRAGMENT_SHADER);

    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error("Error linking program:", gl.getProgramInfoLog(program));
        return null;
    }
    return program;
}

// CanvasRenderingContext3D, string
export function renderShader( gl, fragmentShaderSource, width, height )
{
    // Initialize Shaders
    const program = createProgram(gl, vertexShaderSource, fragmentShaderSource);
    gl.useProgram(program);

    // Create Buffers
    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

    const vertices = new Float32Array([
        -1, -1,  // Bottom left
        1, -1,  // Bottom right
        -1,  1,  // Top left
        1,  1,  // Top right
    ]);
    gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);

    // Set Up Attributes
    const positionLocation = gl.getAttribLocation(program, "aPosition");
    gl.enableVertexAttribArray(positionLocation);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    const iResolutionLocation = gl.getUniformLocation(program, "iResolution");
    gl.uniform3f(iResolutionLocation, width, height, 1);

    // Draw
    function draw() {
        gl.clearColor(0.0, 0.0, 0.0, 1.0); // Black background
        gl.clear(gl.COLOR_BUFFER_BIT);

        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4); // Render quad
    }

    draw();
}