module Intersection

open Matrix
open Ray
open Sphere
open Tuple
open Util

type Intersection = { t: float; object: Sphere }

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

let intersect (ray: Ray) (s: Sphere) =
  let r = Ray.transform (inverse s.transform) ray
  let sphereToRay = r.origin - (point 0. 0. 0.)
  let a = dot r.direction r.direction
  let b = 2. * (dot r.direction sphereToRay)
  let c = (dot sphereToRay sphereToRay) - 1.
  let discriminant = pow 2. b - 4. * a * c
  if (discriminant < 0.)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2. * a)
    let t2 = (-b + sqrt discriminant) / (2. * a)
    [intersection t1 s; intersection t2 s]
