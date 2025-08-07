open Browser
open Three

let scene = Scene.new()

let aspect = innerWidth->Int.toFloat /. innerHeight->Int.toFloat
let camera = PerspectiveCamera.new(~fov=75.0, ~aspect, ~near=0.1, ~far=1000.0)

let renderer = WebGLRenderer.new()

renderer->WebGLRenderer.setSize(~width=innerWidth->Int.toFloat, ~height=innerHeight->Int.toFloat)

let rendererElement = renderer->WebGLRenderer.domElement

Dom.document->Dom.body->Dom.appendChild(~child=rendererElement)

let geometry = BoxGeometry.new(~width=1.0, ~height=1.0, ~depth=1.0)
let material = MeshBasicMaterial.new({"color": 0xF58220})
let cube = Mesh.new(geometry, material)

scene->Scene.add(cube)
camera->PerspectiveCamera.setZ(5.0)

renderer->WebGLRenderer.setAnimationLoop(() => {
  cube->Mesh.setRotationX(cube->Mesh.getRotationX +. 0.01)
  cube->Mesh.setRotationY(cube->Mesh.getRotationY +. 0.01)

  renderer->WebGLRenderer.render(scene, camera)
})
