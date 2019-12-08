module Sphere

open Ray
open Tuple
open Util
open Intersection
open Matrix

type Sphere = { transform: Matrix }
let sphere () = { transform = identity () }

let intersect (s: Sphere) (ray: Ray): Intersection<Sphere> list =
  let r = transform (inverse s.transform) ray
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

let transform t s = { s with transform = t }
