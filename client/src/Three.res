module PerspectiveCamera = {
  type t

  @module("three") @new
  external new: (~fov: float, ~aspect: float, ~near: float, ~far: float) => t = "PerspectiveCamera"

  @set @scope("position")
  external setZ: (t, float) => unit = "z"
}

module BoxGeometry = {
  type t

  @module("three") @new
  external new: (~width: float, ~height: float, ~depth: float) => t = "BoxGeometry"
}

module MeshBasicMaterial = {
  type t

  @module("three") @new
  external new: {"color": int} => t = "MeshBasicMaterial"
}
module Mesh = {
  type t

  // TODO: the arguments for this constructor is more generic in reality
  @module("three") @new
  external new: (BoxGeometry.t, MeshBasicMaterial.t) => t = "Mesh"

  @set @scope("rotation")
  external setRotationX: (t, float) => unit = "x"
  @set @scope("rotation")
  external setRotationY: (t, float) => unit = "y"

  @get @scope("rotation")
  external getRotationX: t => float = "x"
  @get @scope("rotation")
  external getRotationY: t => float = "y"
}

module Scene = {
  type t

  @module("three") @new
  external new: unit => t = "Scene"

  @send
  external add: (t, Mesh.t) => unit = "add"
}

module WebGLRenderer = {
  type t

  @module("three") @new
  external new: unit => t = "WebGLRenderer"

  @send
  external setSize: (t, ~width: float, ~height: float) => unit = "setSize"

  @get
  external domElement: t => Browser.Dom.element = "domElement"

  @send
  external render: (t, Scene.t, PerspectiveCamera.t) => unit = "render"

  @send
  external setAnimationLoop: (t, unit => unit) => unit = "setAnimationLoop"
}
