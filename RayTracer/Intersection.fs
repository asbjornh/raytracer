module Intersection

open Material
open Ray
open Shape
open Tuple
open Util

type Intersection = { t: float; object: IShape }

let getT i = i.t

let intersection t o = { t=t; object=o }

let intersections (l: Intersection list) =
  List.sortBy getT l

let visible i = i.t >= 0.
let hit (l: Intersection list) =
  let visibleEls = List.filter visible l
  match visibleEls with
  | [] -> None
  | _ -> Some (List.minBy getT visibleEls)

type Computation<'a> = {
  t: float
  object: IShape
  point: Tuple
  eyeV: Tuple
  normalV: Tuple
  reflectV: Tuple
  inside: bool
  overPoint: Tuple
  n1: float
  n2: float
}

let indexFromIntersection (s: IShape) =
  match s.Material with
  | Transparent m -> m.index
  | _ -> 1.
let refractiveIndexes (is: Intersection list) (hit: Intersection) =
  is |> List.fold (fun (containers, n1, n2) i ->
    let newN1 =
      if refEq i hit then
        if List.isEmpty containers then 1.
        else indexFromIntersection (List.last containers)
      else n1

    let newC =
      if (containsRef i.object containers) then
        List.filter (fun el -> not (refEq el i.object)) containers
      else List.append containers [i.object]

    let newN2 =
      if refEq i hit then
        if List.isEmpty newC then 1.
        else indexFromIntersection (List.last newC)
      else n2
    (newC, newN1, newN2)
  ) ([], 1., 1.)
  |> (fun (_, n1, n2) -> (n1, n2))


let prepareComputations (is: Intersection list) (hit: Intersection) r =
  let point = position hit.t r
  let normalV = normalAt point hit.object
  let eyeV = negate r.direction
  let inside = (dot normalV eyeV) < 0.
  let normalV =
    (if inside then negate else id) normalV
  let reflectV = reflect normalV r.direction
  let (n1, n2) =
    match hit.object.Material with
    | Transparent _ -> refractiveIndexes is hit
    | _ -> (1., 1.)
  {
    t = hit.t
    object = hit.object
    point = point
    eyeV = eyeV
    normalV = normalV
    reflectV = reflectV
    inside = inside
    overPoint = point + (epsilon * normalV)
    n1 = n1
    n2 = n2
  }

let intersect (ray: Ray) (s: IShape) =
  s |> shapeIntersect ray |> List.map (fun (t, o) -> intersection t o)
