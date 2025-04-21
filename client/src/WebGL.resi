module GL: {
    type s
    type constant
    
    module Buffers: {
        let colorBufferBit: s => constant
    }
    module Program: {
        type t
        type webGLUniformLocation
        let create : s => t
        let link : (s, t) => unit
        let getParameter : (s, t, constant) => bool
        let getAttribLocation : (s, t, string) => int
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
    let attachShader : (s, Program.t, Shader.t) => unit
    let clearColor : (s, float, float, float, float) => unit
    let clear : (s, constant) => unit

    type buffer
    let initBuffers : s => buffer

}

module DOM: {
    let querySelector : string => option<Dom.element>
    let getContext : (Dom.element, string) => option<GL.s>
}

