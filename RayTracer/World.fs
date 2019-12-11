module World

open Intersection
open Light
open Material
open Ray
open Shape

type World = {
  objects: IShape list
  mutable light: Light
}

let world light = {
  objects = [];
  light = light
}

let intersect (ray: Ray) (w: World) =
  w.objects |> List.collect (intersect ray) |> intersections

let shadeHit world comps =
  lighting
    world.light
    comps.point
    comps.eyeV
    comps.normalV
    comps.object.Material

let colorAt world ray =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world
  | None -> Color.black
