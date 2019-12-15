module rec World

open Color
open Intersection
open Light
open Material
open Ray
open Shape
open Transform
open Tuple

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

let shadeHit world comps =
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
    let reflected = reflectedColor comps world
    add surface reflected
  )
  |> List.reduce add

let colorAt world ray =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world
  | None -> world.background

// TODO flip arguments
let reflectedColor comps world =
  let reflective = comps.object.Material.reflective
  if (reflective = 0.)
  then black
  else
    let r = ray comps.overPoint comps.reflectV
    let c = colorAt world r
    Color.scale reflective c
