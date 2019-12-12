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
  mutable light: Light
}

let world light objects = {
  objects = objects
  light = light
}

let defaultWorld () =
  let mat = {
    material () with
      color = color 0.8 1. 0.6;
      diffuse = 0.7
      specular = 0.2
  }
  {
    light = pointLight (point -10. 10. -10.) (color 1. 1. 1.);
    objects = [
      sphereM mat
      sphereT (scaling 0.5 0.5 0.5)
    ]
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
