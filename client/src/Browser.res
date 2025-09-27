@val
external requestAnimationFrame: (float => unit) => unit = "requestAnimationFrame"

@val @scope("window")
external innerWidth: int = "innerWidth"

@val @scope("window")
external innerHeight: int = "innerHeight"

module Dom = {
  type document
  type element
  type canvasElement
  type imageElement

  @val
  external document: document = "document"

  @get
  external body: document => element = "body"

  @send
  external appendChild: (element, ~child: element) => unit = "appendChild"

  @send @return(nullable)
  external querySelector: (document, string) => option<element> = "querySelector"

  @get
  external tagName: element => string = "tagName"

  // TODO: maybe this should be elsewhere
  type pointerEvent

  @get
  external offsetX: pointerEvent => float = "offsetX"
  @get
  external offsetY: pointerEvent => float = "offsetY"
  //

  @send
  external addEventListener: (
    element,
    @string
    [
      | #pointerdown(pointerEvent => unit)
      | #pointermove(pointerEvent => unit)
      | #pointerup(pointerEvent => unit)
      | #load(unit => unit)
    ],
  ) => unit = "addEventListener"

  let toCanvas = (element: element): option<canvasElement> =>
    if element->tagName->String.toLowerCase == "canvas" {
      Some(element->Obj.magic)
    } else {
      None
    }
}

module Canvas = {
  type context2d

  external toElement: Dom.canvasElement => Dom.element = "%identity"

  @set
  external setWidth: (Dom.canvasElement, int) => unit = "width"
  @set
  external setHeight: (Dom.canvasElement, int) => unit = "height"

  @send @return(nullable)
  external getContext2d: (Dom.canvasElement, @as("2d") _) => option<context2d> = "getContext"

  @set
  external fillStyle: (context2d, string) => unit = "fillStyle"

  @send
  external fillRect: (context2d, ~x: float, ~y: float, ~width: float, ~height: float) => unit =
    "fillRect"

  @send
  external drawImage: (
    context2d,
    Dom.imageElement,
    ~dx: float,
    ~dy: float,
    ~dWidth: float,
    ~dHeight: float,
  ) => unit = "drawImage"
}

module Image = {
  external toElement: Dom.imageElement => Dom.element = "%identity"

  @new
  external new: unit => Dom.imageElement = "Image"

  @set
  external setSrc: (Dom.imageElement, string) => unit = "src"
}

module Performance = {
  @val @scope("performance")
  external now: unit => float = "now"
}

module Fetch = {
  type response

  @val
  external get: string => Promise.t<response> = "fetch"

  @send
  external json: response => Promise.t<JSON.t> = "json"
}

module EventSource = {
  type t

  type event = {data: string}

  @new
  external new: string => t = "EventSource"

  @set
  external setOnMessage: (t, event => unit) => unit = "onmessage"
}

module XMLDocument = {
  type t
}

module DOMParser = {
  type t

  @new
  external new: unit => t = "DOMParser"

  @send
  external parseFromString: (t, string, @as("text/xml") _) => XMLDocument.t = "parseFromString"
}
