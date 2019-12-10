module Sphere

open Intersection
open Material
open Matrix
open Ray
open Tuple
open Util

type Sphere = {
  transform: Matrix;
  mutable material: Material;
}
let sphere () = {
  transform = identity ()
  material = material ()
}

let intersect (ray: Ray) (s: Sphere): Intersection<Sphere> list =
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

let normal (p: Tuple) s =
  let invT = inverse s.transform
  let objectN = (multiplyT invT p) - (point 0. 0. 0.)
  let worldN = multiplyT (transpose invT) objectN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)
