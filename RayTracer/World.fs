module rec World

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

let fresnel surface reflect normalV eyeV mat =
  let ang = angle (normalize normalV) (normalize eyeV)
  let amount = ang |> pow 3. |> clamp 0. 1.
  let fColor = blend surface reflect amount
  blend reflect fColor mat.fresnel

let shadeHit world comps remaining =
  world.lights
  |> List.map (fun light ->
    let surface =
      lighting
        light
        comps.point
        comps.eyeV
        comps.normalV
        comps.object.Material
        comps.object.Transform
        (isInShadow comps.overPoint light world.objects)
    let reflected =
      match light with
      | PointLight _ -> reflectedColor world comps remaining
      | ConstantLight _ -> black
    // NOTE: Diverting from book in order to fix reflections for ConstantLight
    let mat = comps.object.Material
    let col = blend surface reflected mat.reflective
    if (mat.fresnel = 0.)
    then col
    else fresnel surface col comps.normalV comps.eyeV mat
  )
  |> List.reduce add

let colorAt world ray remaining =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world <| remaining
  | None -> world.background

let reflectedColor world comps remaining =
  let reflective = comps.object.Material.reflective
  if (remaining < 1 || reflective = 0.)
  then black
  else
    let r = ray comps.overPoint comps.reflectV
    colorAt world r (remaining - 1)
