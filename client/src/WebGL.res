type gl
type glBufferType

@scope("document") @val external _querySelector : string => Nullable.t<Dom.element> = "querySelector"
@send external _getContext : (Dom.element, string) => Nullable.t<gl> = "getContext"
@send external clearColor : (gl, float, float, float, float) => unit = "clearColor"
@get external getColorBufferBit: gl => glBufferType = "COLOR_BUFFER_BIT"
@send external clear : (gl, glBufferType) => unit = "clear"

let querySelector = (str) => { _querySelector(str) |> Nullable.toOption }
let getContext = (elem, str) => { _getContext(elem, str) |> Nullable.toOption }
