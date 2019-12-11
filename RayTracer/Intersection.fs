module Intersection

open Matrix
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
}

let prepareComputations (i: Intersection) r =
  let point = position i.t r
  {
    t = i.t
    object = i.object
    point = point
    eyeV = negate r.direction
    normalV = normal point i.object
  }

let intersect (ray: Ray) (s: IShape) =
  ray |> s.Intersect |> List.map (fun (t, o) -> intersection t o)
