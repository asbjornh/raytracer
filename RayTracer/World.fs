module rec World

open Color
open Intersection
open Light
open Material
open Matrix
open Pattern
open Ray
open Shape
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
      sphereT (scaling 0.5 0.5 0.5)
    ]
  }

let intersectObjects ray (objects: Shape list) =
  objects |> List.collect (Intersection.intersect ray) |> intersections

let intersect (ray: Ray) (w: World) =
  w.objects |> intersectObjects ray

let isInShadow pos objects lightPos =
  let v = lightPos - pos
  let distance = magnitude v
  let direction = normalize v
  let r = ray pos direction
  let intersections = intersectObjects r objects
  let h = hit intersections

  match h with
  | Some hit -> if hit.t < distance then 1. else 0.
  | None -> 0.

let shadowAmount point light objects =
  match light with
  | ConstantLight _ -> 0.
  | PointLight l -> isInShadow point objects l.position 
  | SoftLight l ->
    let count = List.length l.virtualLights
    let hits =
      l.virtualLights |> List.sumBy (isInShadow point objects)
    hits / float count

let shadeTwo world comps remaining matA matB =
  let objectA = { comps.object with material = matA }
  let objectB = { comps.object with material = matB }
  let compsA = { comps with object = objectA }
  let compsB = { comps with object = objectB }
  let a = shadeHit world compsA remaining
  let b = shadeHit world compsB remaining
  getBlendComponents matA matB a b

let shadeHitSingleLight light world comps remaining =
  let objectT = comps.object.transform

  match comps.object.material with
  | Phong mat ->
    let s =
      if world.shadows then
        shadowAmount comps.overPoint light world.objects
      else 0.
    lighting
      light comps.point comps.eyeV comps.normalV mat s

  | Pattern mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let p = patternPoint objectT mat.transform comps.overPoint
    patternAt a b mat.pattern p

  | Textured mat ->
    let p = patternPoint objectT mat.transform comps.overPoint
    let (u, v) = uvAt p comps.object
    Texture.colorAt u v mat.tex

  | Reflective mat ->
    match light with
    | PointLight _ | SoftLight _ -> reflectedColor world comps remaining
    | ConstantLight l ->
      match mat.blend with
      | Normal -> l.intensity | _ -> black

  | Transparent _ ->
    match light with
    | PointLight _ | SoftLight _ -> refractedColor world comps remaining
    | ConstantLight _ -> black

  | Fresnel mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let f = fresnelShade a b comps.normalV comps.eyeV
    mix b f mat.mix

  | Mix mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    mix a b mat.mix

  | Gradient mat ->
    let (a, b) = shadeTwo world comps remaining mat.a mat.b
    let p = patternPoint objectT mat.transform comps.overPoint
    gradientAt a b mat.sharpness p

  | TestPattern ->
    let p = patternPoint objectT (identity ()) comps.overPoint
    let (x, y, z, _) = p.Return
    color x y z

let shadeHit world comps remaining =
  world.lights
  |> List.fold (fun acc light ->
    let singleLightW = { world with lights = [light] }
    let colr = shadeHitSingleLight light singleLightW comps remaining

    match light with
    | PointLight _ | SoftLight _ -> add acc colr
    | ConstantLight l ->
      if l.additive then add acc colr else lighten acc colr
  ) (color 0. 0. 0.)

let colorAt world ray remaining =
  let is = intersect ray world
  match (is |> hit) with
  | Some i -> prepareComputations is i ray |> shadeHit world <| remaining
  | None -> world.background

let colorAndDepthAt world ray remaining =
  let is = intersect ray world
  match (is |> hit) with
  | Some i ->
    let comps = prepareComputations is i ray
    let c = comps |> shadeHit world <| remaining
    (comps.point, comps.normalV, c)
  | None ->
    let p = point infinity infinity infinity
    let n = vector 0. 0. 0.
    (p, n, world.background)

let occlusionAt pos normalV (samples: (Tuple * Tuple)[]) =
  let pointInf = point infinity infinity infinity

  samples
  |> Array.sumBy (fun (pointB, normalB) ->
    if (pointB = pos) then 0.
    else if (pointB = pointInf || pos = pointInf) then 0.
    else
      let d = magnitude (pointB - pos) * 800. * ((abs pos.Z) + 1.)
      let v = pointB - pos |> normalize
      (max 0. (dot normalV v)) * (1. / (1. + d))
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
    let sin2t = nRatio ** 2. * (1. - cosI ** 2.)
    if (sin2t > 1.) then black
    else
      let cosT = sqrt (1. - sin2t)
      let direction = ((nRatio * cosI - cosT) * comps.normalV) - (nRatio * comps.eyeV)
      let r = ray comps.underPoint direction
      colorAt world r (remaining - 1)
