module GL: {
    type s
    type constant
    type webGLBuffer
    type webGLUniformLocation

    module Matrix: {
        let uniformMatrix4fv : (
          s,
          webGLUniformLocation,
          bool,
          Mat4.mat4
        ) => unit
    }

    module Canvas: {
        type t
        let get : s => t
        let clientHeight : t => constant
        let clientWidth : t => constant
    }
    module Buffers: {
        let colorBufferBit: s => constant
        let arrayBuffer : s => constant
        let depthBufferBit: s => constant
    }
    module ProgramInfo: {
        type program
        type attribLocations = { vertexPosition: constant }
        type uniformLocations = {
          projectionMatrix: Nullable.t<webGLUniformLocation>,
          modelViewMatrix: Nullable.t<webGLUniformLocation>
        }
        type t = {
          program: program,
          attribLocations: attribLocations,
          uniformLocations: uniformLocations,
        }
        let program: s => program
        let useProgram : program => unit
        let create : s => t
        let link : (s, t) => unit
        let getParameter : (s, t, constant) => bool
        let getAttribLocation : (s, t, string) => constant
        let getUniformLocation : (s, t, string) => option<webGLUniformLocation>
    }
    module Shader: {
        type t
        let vertex: s => constant
        let fragment: s => constant
        let linkStatus: s => constant
        let create : (s, constant) => option<t>
        let source : (s, t, string) => unit
        let compile : (s, t) => unit
        let getParameter : (s, t, constant) => bool
        let deleteStatus: s => constant
        let compileStatus: s => constant
        let shaderType: s => constant
        let getInfoLog : (s, t) => string
        let delete : (s, t) => unit
    }
    let attachShader : (s, ProgramInfo.t, Shader.t) => unit
    let bindBuffer : (s, constant, webGLBuffer) => unit
    let bufferData : (s, constant, Float32Array.t, constant) => unit
    let vertexAttribPointer : (s, constant, int, constant, bool, int, int) => unit
    let enableVertexAttribArray : (s, constant) => unit
    let clearColor : (s, float, float, float, float) => unit
    let clearDepth : (s, float) => unit
    let depthTest: s => constant
    let float : s => constant
    let depthFunc : (s, constant) => unit
    let lequal : s => constant
    let enable : (s, constant) => unit
    let clear : (s, constant) => unit

    type buffer = { position: webGLBuffer }
    let initBuffers : s => buffer

}

module DOM: {
    let querySelector : string => option<Dom.element>
    let getContext : (Dom.element, string) => option<GL.s>
}

