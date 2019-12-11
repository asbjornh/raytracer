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

let shadeHit w comps =
  lighting w.light comps.point comps.eyeV comps.normalV comps.object.Material
