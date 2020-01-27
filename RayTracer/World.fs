module rec World

open System
open System.Numerics

open Color
open Intersection
open Light
open Material
open Matrix
open Pattern
open Ray
open Shape
open Texture
open Transform
open Tuple
open Util

type World = {
  ambientLight: (Color * BlendingMode) Option
  background: Color
  shadows: bool
  objects: Shape list
  lights: Light list
}

let ambientWorld ambientLight lights objects = {
  ambientLight = ambientLight
  background = black
  shadows = true
  objects = objects
  lights = lights
}

let world lights objects =
  ambientWorld None lights objects

let defaultWorld () =
  {
    ambientLight = None
    background = black
    shadows = true
    lights = [pointLight (point -10. 10. -10.) (color 1. 1. 1.)]
    objects = [
      sphereM (material (color 0.8 1. 0.6) 0.1 0.7 0.2)
      sphereT (uniformScale 0.5f)
    ]
  }

let intersectObjects ray (objects: Shape list) =
  objects |> List.collect (Intersection.intersect ray) |> intersections

let intersect (ray: Ray) (w: World) =
  w.objects |> intersectObjects ray

let isInShadow pos objects lightPos =
  let v = lightPos - pos
  let distance = Vector4.Distance (lightPos, pos)
  let direction = normalize v
  let r = ray pos direction
  let intersections = intersectObjects r objects
  let h = hit intersections

  match h with
  | Some hit -> if hit.t < distance then 1. else 0.
  | None -> 0.

let shadowAmount softShadows point light w =
  if not w.shadows then 0.
  else
  match light with
  | PointLight l ->
      isInShadow point w.objects l.position
  | SoftLight l when (not softShadows) ->
      isInShadow point w.objects l.light.position 
  | SoftLight l ->
      let count = List.length l.virtualLights
      let hits =
        l.virtualLights |> List.sumBy (isInShadow point w.objects)
      hits / float count

// NOTE: the shadow point is shifted for smooth triangles in order to remove some artefacts
// https://computergraphics.stackexchange.com/questions/4986/ray-tracing-shadows-the-shadow-line-artifact
let shadowPoint comps =
  match comps.object.shape with
  | SmoothTriangle t -> comps.overPoint + (0.005f * comps.normalV)
  | _ -> comps.overPoint

let shadeTwo world comps remaining matA matB =
  let objectA = { comps.object with material = matA }
  let objectB = { comps.object with material = matB }
  let compsA = { comps with object = objectA }
  let compsB = { comps with object = objectB }
  let a = shadeHit world compsA remaining
  let b = shadeHit world compsB remaining
  (a, b)

type PixelColor =
  | Constant of Color
  | Component of Color

let getPixelColor = function
  | Constant c -> c
  | Component c -> c
let shadeHitSingleLight light world comps remaining =
  let objectT = comps.object.transform

  match comps.object.material with
  | Phong mat ->
    let shadow =
      shadowAmount mat.softShadows (shadowPoint comps) light world
    lighting
      light comps.point comps.eyeV comps.normalV mat shadow
      |> Component

  | Layer mat ->
    lighting light comps.point comps.eyeV comps.normalV mat 0. |> Component

  | Luminance c -> Constant c

  | Pattern mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let p = patternPoint objectT mat.transform comps.overPoint
    patternAt a b mat.pattern p |> Component

  | Textured mat ->
    let alpha =
      textureOptionAt mat.alpha comps mat.transform mat.uvTransform |> intensity
    if alpha = 0. then
      refractedColor world comps remaining |> Constant
    else
      let baseColor = textureAt comps mat.transform mat.uvTransform mat.color
      let occlusionColor =
        textureOptionAt mat.ambientOcclusion comps mat.transform mat.uvTransform
      let specularAmount =
        textureOptionAt mat.specularMap comps mat.transform mat.uvTransform
        |> invert
      let specular = Color.multiply specularAmount mat.specular
      let newMat = materialShiny baseColor mat.ambient mat.diffuse specular mat.shininess
      let newComps = { comps with object = { comps.object with material = newMat } }
      let surfaceColor =
        shadeHitSingleLight light world newComps remaining |> getPixelColor
        |> Color.multiply occlusionColor
      if alpha = 1. then surfaceColor |> Component
      else Color.mix (refractedColor world comps remaining) surfaceColor alpha |> Component

  | LuminanceTexture mat ->
    textureAt comps mat.transform mat.uvTransform mat.tex |> Constant

  | NormalMap mat ->
    let normalV =
      textureAt comps mat.transform mat.uvTransform mat.tex
      |> normalFromColor comps.normalV |> normalize
    let reflectV = reflect normalV comps.ray.direction

    let newObj = { comps.object with material = mat.mat }
    let newComps = { comps with normalV = normalV; reflectV = reflectV; object = newObj }
    shadeHit world newComps remaining
    |> Component

  | Reflective mat ->
    reflectedColor mat world comps remaining |> Constant

  | Transparent _ ->
    refractedColor world comps remaining |> Component

  | Fresnel mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let b2 = blend mat.blend a b
    let a3 = mix b2 a mat.mixInner
    let b3 = mix a b2 mat.mixOuter
    fresnelShade a3 b3 mat.power comps.normalV comps.eyeV |> Component

  | Mix mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    mix a b mat.mix |> Component

  | Blend mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    blend mat.mode a b |> Component

  | Gradient mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let p = patternPoint objectT mat.transform comps.overPoint
    gradientAt a b mat.sharpness p |> Component

  | InvisFloor mat ->
    let s = shadowAmount true comps.overPoint light world
    let r = ray comps.underPoint (negate comps.eyeV)
    let c = colorAt world r (remaining - 1)
    mix c mat.shadowColor s |> Constant

  | TestPattern ->
    let p = patternPoint objectT identity comps.overPoint
    color (float p.X) (float p.Y) (float p.Z) |> Component

let shadeHit world comps remaining =
  world.lights
  |> List.fold (fun acc light ->
    let singleLightW = { world with lights = [light] }
    let colr = shadeHitSingleLight light singleLightW comps remaining

    match colr with
    | Constant c -> c
    | Component c -> add acc c
  ) (color 0. 0. 0.)

let colorAndAmbientAt world ray remaining =
  let color =
    colorAt world ray remaining |> Color.MapC (clamp 0. 1.)
  match world.ambientLight with
  | None -> color
  | Some (c, mode) -> blend mode c color

let colorAt world ray remaining =
  let is = intersect ray world
  match (is |> hit) with
  | Some i -> prepareComputations is i ray |> shadeHit world <| remaining
  | None -> world.background

let textureAt comps (transform: Matrix4x4) uvTransform tex =
  let p = patternPoint comps.object.transform transform comps.overPoint
  let (u, v) = uvAt p comps.object
  Texture.colorAt u v uvTransform tex

let textureOptionAt texture comps transform uvTransform =
  match texture with
    | None -> white
    | Some tex -> textureAt comps transform uvTransform tex

let occlusionAt numSamples threshold world r =
  let is = intersect r world
  match (is |> hit) with
  | Some i ->
    let comps = prepareComputations is i r
    [0..numSamples]
    |> List.averageBy (fun _ ->
      let t = randomRotate <| Math.PI / 2.
      let r2 = ray comps.overPoint (transform t comps.normalV)
      match (intersect r2 world |> hit) with
      | Some i -> if i.t < threshold then 1. else 0.
      | None -> 0.
    )
  | None -> 0.

let reflectedColor mat world comps remaining =
  if (remaining < 1) then black
  else 
    match mat with
    | None ->
      let r = ray comps.overPoint comps.reflectV
      colorAt world r (remaining - 1)
    | Some blurOptions ->
      [0..blurOptions.samples]
      |> List.map (fun _ ->
        let t = randomRotate <| blurOptions.angle
        let r = ray comps.overPoint (transform t comps.reflectV)
        colorAt world r (remaining - 1)
      )
      |> Color.average

let refractedColor world comps remaining =
  if (remaining < 1) then black
  else
    let nRatio = comps.n1 / comps.n2
    let cosI = dot comps.eyeV comps.normalV
    let sin2t = nRatio ** 2.f * (1.f - cosI ** 2.f)
    if (sin2t > 1.f) then black
    else
      let cosT = sqrt (1.f - sin2t)
      let direction = ((nRatio * cosI - cosT) * comps.normalV) - (nRatio * comps.eyeV)
      let r = ray comps.underPoint direction
      colorAt world r (remaining - 1)
