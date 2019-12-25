module Plane

open Ray
open Util
open Tuple

let uv (p: Tuple) =
  let scale = 1. / 10.
  (p.X * scale, p.Z * scale)

let intersect ray s =
  if (looseEq ray.direction.Y 0.)
  then []
  else
    let t = -ray.origin.Y / ray.direction.Y
    let x = ray.origin.X + t * ray.direction.X
    let z = ray.origin.Z + t * ray.direction.Z
    if (x < -1. || x > 1.) then []
    else if (z < -1. || z > 1.) then []
    else [(t, s)]
