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
  let mat = {
    material () with
      color = color 0.8 1. 0.6;
      diffuse = 0.7
      specular = 0.2
  }
  {
    lights = [pointLight (point -10. 10. -10.) (color 1. 1. 1.)]
    objects = [
      sphereM mat
      sphereT (scaling 0.5 0.5 0.5)
    ]
  }

let intersect (ray: Ray) (w: World) =
  w.objects |> List.collect (intersect ray) |> intersections

let shadeHit world comps =
  world.lights
  |> List.map (fun light ->
    lighting
      light
      comps.point
      comps.eyeV
      comps.normalV
      comps.object.Material
  )
  |> List.reduce add

let colorAt world ray =
  match (intersect ray world |> hit) with
  | Some i -> prepareComputations i ray |> shadeHit world
  | None -> Color.black
