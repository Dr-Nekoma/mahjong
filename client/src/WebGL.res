module GL = {
    type s
    type constant = float
    module Buffers = {
        @get external colorBufferBit: s => constant = "COLOR_BUFFER_BIT"
    }
    module Program = {
        type t
        @send external create : s => t = "createProgram"
        @send external link : (s, t) => unit = "linkProgram"
        @send external getParameter : (s, t, constant) => bool = "getProgramParameter"
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
    @send external attachShader : (s, Program.t, Shader.t) => unit = "attachShader"
    @send external clearColor : (s, float, float, float, float) => unit = "clearColor"
    @send external clear : (s, float) => unit = "clear"

}

module DOM = {
    @scope("document") @val external _querySelector : string => Nullable.t<Dom.element> = "querySelector"
    @send external _getContext : (Dom.element, string) => Nullable.t<GL.s> = "getContext"

    let querySelector = (str) => { _querySelector(str) |> Nullable.toOption }
    let getContext = (elem, str) => { _getContext(elem, str) |> Nullable.toOption }
}

