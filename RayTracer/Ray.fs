module Ray

open Util
open Tuple

type Ray = {
  origin: Tuple;
  direction: Tuple;
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position t ray =
  ray.origin + t * ray.direction

let intersect s ray =
  let sphereToRay = ray.origin - (point 0. 0. 0.)
  let a = ray.direction /. ray.direction
  let b = 2. * (ray.direction /. sphereToRay)
  let c = (sphereToRay /. sphereToRay) - 1.
  let discriminant = pow 2. b - 4. * a * c
  if (discriminant < 0.)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2. * a)
    let t2 = (-b + sqrt discriminant) / (2. * a)
    [t1; t2]
