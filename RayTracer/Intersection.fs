module Intersection

open System.Numerics

open Material
open Ray
open Shape
open Tuple
open Util

type Intersection = {
  t: float32;
  object: Shape
}

let getT i = i.t

let intersection t o = { t=t; object=o }

let intersections (l: Intersection list) =
  List.sortBy getT l

let visible i = i.t >= 0.f
let hit (l: Intersection list) =
  match List.filter visible l with
  | [] -> None
  | visibleShapes -> Some (List.minBy getT visibleShapes)

type Computation = {
  t: float32
  object: Shape
  point: Tuple
  eyeV: Tuple
  normalV: Tuple
  reflectV: Tuple
  inside: bool
  overPoint: Tuple
  underPoint: Tuple
  n1: float32
  n2: float32
}

let pickIndex (a: float32) b =
  if (a = b) then a
  else if (a = 1.f) then b
  else a

let rec indexForMaterial = function
  | Transparent m -> m.index
  | Mix m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Blend m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Fresnel m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Gradient _ | Pattern _ | Phong _ | NormalMap _ | Luminance _
  | Reflective _ | TestPattern _ | Textured _ -> 1.f

let indexFromIntersection (s: Shape) =
  indexForMaterial s.material

let refractiveIndexes (is: Intersection list) (hit: Intersection) =
  is |> List.fold (fun (containers, n1, n2) i ->
    let newN1 =
      if not (refEq i hit) then n1
      else if List.isEmpty containers then 1.f
      else indexFromIntersection (List.last containers)

    let newC =
      if (containsRef i.object containers) then
        List.filter (fun el -> not (refEq el i.object)) containers
      else List.append containers [i.object]

    let newN2 =
      if not (refEq i hit) then n2
      else if List.isEmpty newC then 1.f
      else indexFromIntersection (List.last newC)
    (newC, newN1, newN2)
  ) ([], 1.f, 1.f)
  |> (fun (_, n1, n2) -> (n1, n2))


let prepareComputations (is: Intersection list) (hit: Intersection) r =
  let point = position hit.t r
  let normalV = normalAt hit.object point
  let eyeV = negate r.direction
  let inside = (dot normalV eyeV) < 0.f
  let normalV =
    (if inside then negate else id) normalV
  let reflectV = reflect normalV r.direction
  let (n1, n2) = refractiveIndexes is hit
  // NOTE: Scaling epsilon by the distance to origin fixes some shadow acne
  let d = Vector4.Distance (r.origin.Vec, point.Vec)
  {
    t = hit.t
    object = hit.object
    point = point
    eyeV = eyeV
    normalV = normalV
    reflectV = reflectV
    inside = inside
    overPoint = point + (d * epsilon32 * normalV)
    underPoint = point - (d * epsilon32 * normalV)
    n1 = n1
    n2 = n2
  }

let intersect (ray: Ray) (s: Shape) =
  s |> shapeIntersect ray |> List.map (fun (t, o) -> intersection t o)
