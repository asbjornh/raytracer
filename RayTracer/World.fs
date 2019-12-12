module World

open Color
open Intersection
open Light
open Material
open Ray
open Shape
open Transform
open Tuple

type World = {
  objects: IShape list
  lights: Light list
}

let world lights objects = {
  objects = List.map (fun o -> (o :> IShape)) objects
  lights = lights
}

let defaultWorld () =
  {
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

let shadeHit world comps =
  world.lights
  |> List.map (fun light ->
    lighting
      light
      comps.point
      comps.eyeV
      comps.normalV
      comps.object.Material
      false // TODO
  )
  |> List.reduce add

let colorAt world ray =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world
  | None -> Color.black

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
