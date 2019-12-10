module World

open Intersection
open Light
open Ray
open Sphere

type World = {
  objects: Sphere list
  mutable light: Light
}

let world light = {
  objects = [];
  light = light
}

let intersect (ray: Ray) (w: World) =
  w.objects |> List.collect (intersect ray) |> intersections
