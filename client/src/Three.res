type colorSpace

@module("three")
external srgbColorSpace: colorSpace = "SRGBColorSpace"

module PerspectiveCamera = {
  type t

  @module("three") @new
  external new: (~fov: float, ~aspect: float, ~near: float, ~far: float) => t = "PerspectiveCamera"

  @set @scope("position")
  external setZ: (t, float) => unit = "z"

  @set @scope("position")
  external setY: (t, float) => unit = "y"

  @send
  external lookAt: (t, float, float, float) => unit = "lookAt"
}

module BoxGeometry = {
  type t

  @module("three") @new
  external new: (~width: float, ~height: float, ~depth: float) => t = "BoxGeometry"
}

module Texture = {
  type t

  @set
  external setColorSpace: (t, colorSpace) => unit = "colorSpace"
}

module TextureLoader = {
  type t

  @module("three") @new
  external new: unit => t = "TextureLoader"

  @send
  external load: (t, string) => Texture.t = "load"
}

module MeshStandardMaterial = {
  type t

  // TODO: this should be hidden
  @obj external makeNew: (~map: Texture.t=?, ~color: int=?, ~transparent: bool=?) => {.} = ""

  @module("three") @new
  external new: {.} => t = "MeshStandardMaterial"

  let new = (~map: option<Texture.t>=?, ~color: option<int>=?, ~transparent: option<bool>=?) => {
    makeNew(~map?, ~color?, ~transparent?)->new
  }
}

module PlaneGeometry = {
  type t

  @module("three") @new
  external new: (~width: float, ~height: float) => t = "PlaneGeometry"
}

module Mesh = {
  type t

  // TODO: the arguments for this constructor is more generic in reality
  @module("three") @new
  external newBox: (BoxGeometry.t, MeshStandardMaterial.t) => t = "Mesh"

  @module("three") @new
  external newPlane: (PlaneGeometry.t, MeshStandardMaterial.t) => t = "Mesh"

  @module("three") @new
  external newManyMaterials: (BoxGeometry.t, array<MeshStandardMaterial.t>) => t = "Mesh"

  @set @scope("rotation")
  external setRotationX: (t, float) => unit = "x"
  @set @scope("rotation")
  external setRotationY: (t, float) => unit = "y"

  @get @scope("rotation")
  external getRotationX: t => float = "x"
  @get @scope("rotation")
  external getRotationY: t => float = "y"

  @set @scope("position")
  external setPositionX: (t, float) => unit = "x"
  @set @scope("position")
  external setPositionY: (t, float) => unit = "y"
  @set @scope("position")
  external setPositionZ: (t, float) => unit = "z"

  @get @scope("position")
  external getPositionX: t => float = "x"
  @get @scope("position")
  external getPositionY: t => float = "y"

  @set
  external setCastShadow: (t, bool) => unit = "castShadow"

  @set
  external setReceiveShadow: (t, bool) => unit = "receiveShadow"
}

module Color = {
  type t

  @module("three") @new
  external new: int => t = "Color"
}

module AmbientLight = {
  type t

  @module("three") @new
  external new: (~color: int, ~intensity: float) => t = "AmbientLight"
}

module DirectionalLight = {
  type t

  @module("three") @new
  external new: (~color: int, ~intensity: float) => t = "DirectionalLight"

  @send @scope("position")
  external setPosition: (t, ~x: float, ~y: float, ~z: float) => unit = "set"

  @set
  external setCastShadow: (t, bool) => unit = "castShadow"
}

module Scene = {
  type t

  @module("three") @new
  external new: unit => t = "Scene"

  @send
  external addMesh: (t, Mesh.t) => unit = "add"

  @send
  external addAmbientLight: (t, AmbientLight.t) => unit = "add"
  @send
  external addDirectionalLight: (t, DirectionalLight.t) => unit = "add"

  @set
  external setBackground: (t, Color.t) => unit = "background"
}

type shadowType

@module("three")
external pcfSoftShadowMap: shadowType = "PCFSoftShadowMap"

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

  @set @scope("shadowMap")
  external shadowMapSetEnabled: (t, bool) => unit = "enabled"

  @set @scope("shadowMap")
  external shadowMapSetType: (t, shadowType) => unit = "type"
}
