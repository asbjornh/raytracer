module Intersection

open Material
open Ray
open Shape
open Tuple
open Util

type Intersection = {
  t: float;
  object: Shape
}

let getT i = i.t

let intersection t o = { t=t; object=o }

let intersections (l: Intersection list) =
  List.sortBy getT l

let visible i = i.t >= 0.
let hit (l: Intersection list) =
  match List.filter visible l with
  | [] -> None
  | visibleShapes -> Some (List.minBy getT visibleShapes)

type Computation<'a> = {
  t: float
  object: Shape
  point: Tuple
  eyeV: Tuple
  normalV: Tuple
  reflectV: Tuple
  inside: bool
  overPoint: Tuple
  underPoint: Tuple
  n1: float
  n2: float
}

let pickIndex (a: float) b =
  if (a = b) then a
  else if (a = 1.) then b
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
  | Reflective _ | TestPattern _ | Textured _ -> 1.

let indexFromIntersection (s: Shape) =
  indexForMaterial s.material

let refractiveIndexes (is: Intersection list) (hit: Intersection) =
  is |> List.fold (fun (containers, n1, n2) i ->
    let newN1 =
      if not (refEq i hit) then n1
      else if List.isEmpty containers then 1.
      else indexFromIntersection (List.last containers)

    let newC =
      if (containsRef i.object containers) then
        List.filter (fun el -> not (refEq el i.object)) containers
      else List.append containers [i.object]

    let newN2 =
      if not (refEq i hit) then n2
      else if List.isEmpty newC then 1.
      else indexFromIntersection (List.last newC)
    (newC, newN1, newN2)
  ) ([], 1., 1.)
  |> (fun (_, n1, n2) -> (n1, n2))


let prepareComputations (is: Intersection list) (hit: Intersection) r =
  let point = position hit.t r
  let normalV = normalAt hit.object point
  let eyeV = negate r.direction
  let inside = (dot normalV eyeV) < 0.
  let normalV =
    (if inside then negate else id) normalV
  let reflectV = reflect normalV r.direction
  let (n1, n2) = refractiveIndexes is hit
  {
    t = hit.t
    object = hit.object
    point = point
    eyeV = eyeV
    normalV = normalV
    reflectV = reflectV
    inside = inside
    overPoint = point + (epsilon * normalV)
    underPoint = point - (epsilon * normalV)
    n1 = n1
    n2 = n2
  }

let intersect (ray: Ray) (s: Shape) =
  s |> shapeIntersect ray |> List.map (fun (t, o) -> intersection t o)
