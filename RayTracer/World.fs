module rec World

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
  background: Color
  shadows: bool
  objects: Shape list
  lights: Light list
}

let world lights objects = {
  background = black
  shadows = true
  objects = objects
  lights = lights
}

let defaultWorld () =
  {
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

let shadowAmount point light w =
  if not w.shadows then 0.
  else
  match light with
  | ConstantLight _ -> 0.
  | PointLight l -> isInShadow point w.objects l.position 
  | SoftLight l ->
    let count = List.length l.virtualLights
    let hits =
      l.virtualLights |> List.sumBy (isInShadow point w.objects)
    hits / float count

let shadeTwo world comps remaining matA matB =
  let objectA = { comps.object with material = matA }
  let objectB = { comps.object with material = matB }
  let compsA = { comps with object = objectA }
  let compsB = { comps with object = objectB }
  let a = shadeHit world compsA remaining
  let b = shadeHit world compsB remaining
  getBlendComponents matA matB a b

type PixelColor =
  | Constant of Color
  | Component of Color
  | Blended of BlendingMode * Color
let shadeHitSingleLight light world comps remaining =
  let objectT = comps.object.transform

  match comps.object.material with
  | Phong mat ->
    let s = shadowAmount comps.overPoint light world
    lighting
      light comps.point comps.eyeV comps.normalV mat s
      |> Component

  | Luminance c -> Constant c

  | Pattern mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let p = patternPoint objectT mat.transform comps.overPoint
    patternAt a b mat.pattern p |> Component

  | Textured mat ->
    textureAt comps mat |> Constant

  | NormalMap mat ->
    let normalV =
      textureAt comps mat.tex
      |> Color.MapC (rangeMap (0., 1.) (-1., 1.))
      |> mappedNormalAt comps.normalV |> normalize

    let newObj = { comps.object with material = mat.mat }
    shadeHit world { comps with normalV = normalV; object = newObj } remaining
    |> Component

  | Reflective mat ->
    match light with
    | PointLight _ | SoftLight _ ->
      reflectedColor world comps remaining |> Component
    | ConstantLight l ->
      match mat.blend with
      | Normal -> l.intensity | _ -> black
      |> Component

  | Transparent _ ->
    match light with
    | PointLight _ | SoftLight _ -> refractedColor world comps remaining
    | ConstantLight _ -> black
    |> Component

  | Fresnel mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let a2 = mix b a mat.mixInner
    let b2 = mix a b mat.mixOuter
    fresnelShade a2 b2 mat.power comps.normalV comps.eyeV |> Component

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
    let s = shadowAmount comps.overPoint light world
    let r = ray comps.underPoint (negate comps.eyeV)
    let c = colorAt world r (remaining - 1)
    match light with
    | ConstantLight _ -> Blended (Add, black)
    | _ -> mix c mat.shadowColor s |> Constant

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
    | Blended (mode, c) -> blend mode acc c
    | Component c ->
      match light with
      | PointLight _ | SoftLight _ -> add acc c
      | ConstantLight l ->
        blend l.mode acc c
  ) (color 0. 0. 0.)

let colorAt world ray remaining =
  let is = intersect ray world
  match (is |> hit) with
  | Some i -> prepareComputations is i ray |> shadeHit world <| remaining
  | None -> world.background

let textureAt comps (mat: Textured) =
  let p = patternPoint comps.object.transform mat.transform comps.overPoint
  let (u, v) = uvAt p comps.object
  Texture.colorAt u v mat.uScale mat.vScale mat.uOffset mat.vOffset mat.tex

let depthAt world ray =
  let is = intersect ray world
  match (is |> hit) with
  | Some i ->
    let comps = prepareComputations is i ray
    (comps.point, comps.normalV)
  | None ->
    let p = point infinity infinity infinity
    let n = vector 0. 0. 0.
    (p, n)

let occlusionAt pos normalV (samples: (Vector4 * Vector4)[]) =
  let pointInf = point infinity infinity infinity

  samples
  |> Array.sumBy (fun (pointB, normalB) ->
    if (fastEquals pointB pos) then 0.f
    else if (fastEquals pointB pointInf) then 0.f
    else if (fastEquals pos pointInf) then 0.f
    else
      let d = Vector4.Distance (pointB, pos) * 400.f * ((abs pos.Z) + 1.f)
      let v = pointB - pos |> normalize
      (max 0.f (dot normalV v)) * (1.f / (1.f + d))
  )

let reflectedColor world comps remaining =
  if (remaining < 1) then black
  else
    let r = ray comps.overPoint comps.reflectV
    colorAt world r (remaining - 1)

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
