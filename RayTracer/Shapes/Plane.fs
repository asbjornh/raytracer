module Plane

open Ray
open Util
open Tuple

let uv (p: Tuple) =
  let scale = 1.f / 10.f
  (p.X * scale, p.Z * scale)

let intersect ray s =
  if (looseEq32 ray.direction.Y 0.f)
  then []
  else
    let t = -ray.origin.Y / ray.direction.Y
    let x = ray.origin.X + t * ray.direction.X
    let z = ray.origin.Z + t * ray.direction.Z
    if (x < -1.f || x > 1.f) then []
    else if (z < -1.f || z > 1.f) then []
    else [(t, s)]
