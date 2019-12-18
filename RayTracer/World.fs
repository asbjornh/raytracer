module rec World

open System

open Color
open Intersection
open Light
open Material
open Ray
open Shape
open Transform
open Tuple
open Util

type World = {
  background: Color
  objects: IShape list
  lights: Light list
}

let world lights objects = {
  background = black
  objects = List.map (fun o -> (o :> IShape)) objects
  lights = lights
}

let defaultWorld () =
  {
    background = black
    lights = [pointLight (point -10. 10. -10.) (color 1. 1. 1.)]
    objects = [
      sphereM (material (color 0.8 1. 0.6) 0.1 0.7 0.2)
      sphereT (scaling 0.5 0.5 0.5)
    ]
  }

let intersectObjects ray (objects: IShape list) =
  objects |> List.collect (Intersection.intersect ray) |> intersections

let intersect (ray: Ray) (w: World) =
  w.objects |> intersectObjects ray

let isInShadow point light objects =
  match light with
  | ConstantLight _ -> false
  | PointLight l ->
    let v = l.position - point
    let distance = magnitude v
    let direction = normalize v
    let r = ray point direction
    let intersections = intersectObjects r objects
    let h = hit intersections

    match h with
    | Some hit -> hit.t < distance
    | None -> false

let shadeHit world comps remaining =
  world.lights
  |> List.fold (fun acc light ->
    let singleLightW = { world with lights = [light] }
    let colr =
      match comps.object.Material with
      | Phong mat ->
        lighting
          light
          comps.point
          comps.eyeV
          comps.normalV
          mat
          comps.object.Transform
          (isInShadow comps.overPoint light world.objects)
      | Reflective _ ->
        match light with
        | PointLight _ -> reflectedColor singleLightW comps remaining
        | ConstantLight l -> black
      | Fresnel mat ->
        let compsA = { comps with object = assignMaterial comps.object mat.a }
        let compsB = { comps with object = assignMaterial comps.object mat.b }
        let a = shadeHit singleLightW compsA remaining
        let b = shadeHit singleLightW compsB remaining
        let f = fresnelShade a b comps.normalV comps.eyeV mat.a mat.b
        blend a f mat.mix
      | Blend mat ->
        let compsA = { comps with object = assignMaterial comps.object mat.a }
        let compsB = { comps with object = assignMaterial comps.object mat.b }
        let a = shadeHit singleLightW compsA remaining
        let b = shadeHit singleLightW compsB remaining
        blend a b mat.mix

    match light with
    | PointLight _ -> add acc colr
    | ConstantLight _ -> lighten acc colr
  ) (color 0. 0. 0.)

let colorAt world ray remaining =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world <| remaining
  | None -> world.background

let colorAndDepthAt world ray remaining =
  match (intersect ray world |> hit) with
  | Some i ->
    let comps = prepareComputations i ray
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
