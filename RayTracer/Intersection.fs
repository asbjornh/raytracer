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
  let mutable containers: IShape list = []
  let mutable n1 = 1.
  let mutable n2 = 1.

  is |> List.iter (fun i ->
    if refEq i hit then
      if List.isEmpty containers then
        n1 <- 1.
      else
        n1 <- indexFromIntersection (List.last containers)
    else ignore ()

    if (containsRef i.object containers) then
      containers <-
        List.filter (fun el -> not (refEq el i.object)) containers
    else
      containers <-
        List.append containers [i.object]
    
    if refEq i hit then
      if List.isEmpty containers then
        n2 <- 1.
      else
        n2 <- indexFromIntersection (List.last containers)
    else ignore ()
  )
  (n1, n2)

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
