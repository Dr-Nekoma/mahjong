open Browser
open Three

let scene = Scene.new()
scene->Scene.setBackground(Color.new(0xffffff))

let aspect = innerWidth->Int.toFloat /. innerHeight->Int.toFloat
let camera = PerspectiveCamera.new(~fov=75.0, ~aspect, ~near=0.1, ~far=1000.0)

let renderer = WebGLRenderer.new()
renderer->WebGLRenderer.setSize(~width=innerWidth->Int.toFloat, ~height=innerHeight->Int.toFloat)
renderer->WebGLRenderer.shadowMapSetEnabled(true)
renderer->WebGLRenderer.shadowMapSetType(pcfSoftShadowMap)

let rendererElement = renderer->WebGLRenderer.domElement

Dom.document->Dom.body->Dom.appendChild(~child=rendererElement)

let pieceGeometry = BoxGeometry.new(~width=2.0, ~height=3.0, ~depth=1.0)

let textureLoader = TextureLoader.new()

let frontTexture = textureLoader->TextureLoader.load("assets/ton.png")
// texture->Texture.setColorSpace(Three.srgbColorSpace)

let plainMaterial = MeshStandardMaterial.new(~color=0xffffff)
let frontMaterial = MeshStandardMaterial.new(~map=frontTexture, ~color=0xffffff)

let piece1 = Mesh.newManyMaterials(
  pieceGeometry,
  [plainMaterial, plainMaterial, plainMaterial, plainMaterial, frontMaterial, plainMaterial],
)
let piece2 = Mesh.newManyMaterials(
  pieceGeometry,
  [plainMaterial, plainMaterial, plainMaterial, plainMaterial, frontMaterial, plainMaterial],
)

piece1->Mesh.setCastShadow(true)
piece1->Mesh.setReceiveShadow(true)

piece2->Mesh.setCastShadow(true)
piece2->Mesh.setReceiveShadow(true)

piece1->Mesh.setPositionY(5.0)
piece2->Mesh.setPositionY(-5.0)

piece1->Mesh.setRotationX(Math.Constants.pi /. 2.0)
piece2->Mesh.setRotationX(Math.Constants.pi /. 2.0)

scene->Scene.addMesh(piece1)
scene->Scene.addMesh(piece2)

let floorGeometry = PlaneGeometry.new(~width=20.0, ~height=20.0)
let floorMaterial = MeshStandardMaterial.new(~color=0x207340)
let floor = Mesh.newPlane(floorGeometry, floorMaterial)
floor->Mesh.setPositionZ(-2.0)
floor->Mesh.setReceiveShadow(true)
scene->Scene.addMesh(floor)

let ambientLight = AmbientLight.new(~color=0xffffff, ~intensity=0.3)
scene->Scene.addAmbientLight(ambientLight)

let directionalLight = DirectionalLight.new(~color=0xffffff, ~intensity=1.0)
directionalLight->DirectionalLight.setPosition(~x=2.0, ~y=-5.0, ~z=5.0)
directionalLight->DirectionalLight.setCastShadow(true)
scene->Scene.addDirectionalLight(directionalLight)

camera->PerspectiveCamera.setZ(20.0)
camera->PerspectiveCamera.setY(-15.0)

camera->PerspectiveCamera.lookAt(0.0, 0.0, 0.0)

renderer->WebGLRenderer.setAnimationLoop(() => {
  renderer->WebGLRenderer.render(scene, camera)
})

type joke = {
  setup: string,
  delivery: string,
}

let jsonToJoke = (json: JSON.t): option<joke> => {
  switch json {
  | Object(jokeDict) =>
    switch (jokeDict->Core__Dict.get("setup"), jokeDict->Core__Dict.get("delivery")) {
    | (Some(String(setup)), Some(String(delivery))) => Some({setup, delivery})
    | _ => None
    }
  | _ => None
  }
}

@val
external alert: string => unit = "alert"

// Fetch.get("https://sv443.net/jokeapi/v2/joke/Programming")
// ->Promise.then(response => response->Fetch.json)
// ->Promise.thenResolve(json =>
//   jsonToJoke(json)->Option.forEach(joke => {
//     alert(joke.setup)
//     alert(joke.delivery)
//   })
// )
// ->Promise.done
