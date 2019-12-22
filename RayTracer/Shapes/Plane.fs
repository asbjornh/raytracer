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
  else [(-ray.origin.Y / ray.direction.Y, s)]
