module GL = {
    type s
    type constant = float
    type webGLBuffer
    type location
    module Matrix = {
        @send external uniformMatrix4fv : (location, bool, Mat4.mat4) => unit = "uniformMatrix4fv"

    }
    module Canvas = {
        type t
        @get external get : s => t = "canvas"
        @get external clientHeight : t => constant = "clientHeight"
        @get external clientWidth : t => constant = "clientWidth"
    }
    module Buffers = {
        @get external colorBufferBit: s => constant = "COLOR_BUFFER_BIT"
        @get external arrayBuffer: s => constant = "ARRAY_BUFFER"
        @get external depthBufferBit: s => constant = "DEPTH_BUFFER_BIT"
    }
    module ProgramInfo = {
        type t
        type program
        type webGLUniformLocation
        @get external program: s => program = "program"
        @send external useProgram : program => unit = "useProgram"
        @send external create : s => t = "createProgram"
        @send external link : (s, t) => unit = "linkProgram"
        @send external getParameter : (s, t, constant) => bool = "getProgramParameter"
        @send external getAttribLocation : (s, t, string) => int = "getAttribLocation"
        @send external _getUniformLocation : (s, t, string) => Nullable.t<webGLUniformLocation> = "getUniformLocation"
        let getUniformLocation = (s, t, string) => { _getUniformLocation(s, t, string) |> Nullable.toOption }

    }
    module Shader = {
        type t
        @get external vertex: s => constant = "VERTEX_SHADER"
        @get external fragment: s => constant = "FRAGMENT_SHADER"
        @get external linkStatus: s => constant = "LINK_STATUS"
        @send external _create : (s, constant) => Nullable.t<t> = "createShader"
        let create = (s, const) => { _create(s, const) |> Nullable.toOption }
        @send external source : (s, t, string) => unit = "shaderSource"
        @send external compile : (s, t) => unit = "compileShader"
        @send external getParameter : (s, t, constant) => bool = "getShaderParameter"
        @get external deleteStatus: s => constant = "DELETE_STATUS"
        @get external compileStatus: s => constant = "COMPILE_STATUS"
        @get external shaderType: s => constant = "SHADER_TYPE"
        @send external getInfoLog : (s, t) => string = "getShaderInfoLog"
        @send external delete : (s, t) => unit = "deleteShader"
    }
    @send external attachShader : (s, ProgramInfo.t, Shader.t) => unit = "attachShader"
    @send external clearColor : (s, float, float, float, float) => unit = "clearColor"
    @send external clearDepth : (s, constant) => unit = "clearDepth"
    @get external depthTest: s => constant = "DEPTH_TEST"
    @send external depthFunc : (s, constant) => unit = "depthFunc"
    @get external lequal: s => constant = "LEQUAL"
    @send external enable : (s, constant) => unit = "enable"
    @send external clear : (s, float) => unit = "clear"
    @send external createBuffer : s => webGLBuffer = "createBuffer"
    @send external bindBuffer : (s, constant, webGLBuffer) => unit = "bindBuffer"
    @send external bufferData : (s, constant, Float32Array.t, constant) => unit = "bufferData"
    @get external staticDraw: s => constant = "STATIC_DRAW"

    let initPositionBuffer = (gl: s): webGLBuffer => {
        // Create a buffer for the square's positions.
        let positionBuffer = createBuffer(gl);

        // Select the positionBuffer as the one to apply buffer
        // operations to from here out.
        bindBuffer(gl, Buffers.arrayBuffer(gl), positionBuffer);

        // Now create an array of positions for the square.
        let positions = [1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, -1.0];

        // Now pass the list of positions into WebGL to build the
        // shape. We do this by creating a Float32Array from the
        // JavaScript array, then use it to fill the current buffer.
        bufferData(gl, Buffers.arrayBuffer(gl), Float32Array.fromArray(positions), staticDraw(gl));

        positionBuffer;
    }

    type buffer = {
        "position": webGLBuffer
    };

    let initBuffers = (gl: s): buffer => {
        let positionBuffer = initPositionBuffer(gl);

        {
            "position": positionBuffer,
        };
    }

}

module DOM = {
    @scope("document") @val external _querySelector : string => Nullable.t<Dom.element> = "querySelector"
    @send external _getContext : (Dom.element, string) => Nullable.t<GL.s> = "getContext"

    let querySelector = (str) => { _querySelector(str) |> Nullable.toOption }
    let getContext = (elem, str) => { _getContext(elem, str) |> Nullable.toOption }
}

