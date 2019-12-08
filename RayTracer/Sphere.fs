module Sphere

open Ray
open Tuple
open Util
open Intersection

type Sphere = Sphere of unit
let sphere () = Sphere ()

let intersect (s: Sphere) (ray: Ray): Intersection<Sphere> list =
  let sphereToRay = ray.origin - (point 0. 0. 0.)
  let a = dot ray.direction ray.direction
  let b = 2. * (dot ray.direction sphereToRay)
  let c = (dot sphereToRay sphereToRay) - 1.
  let discriminant = pow 2. b - 4. * a * c
  if (discriminant < 0.)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2. * a)
    let t2 = (-b + sqrt discriminant) / (2. * a)
    [intersection t1 s; intersection t2 s]
