module Plane

open Ray
open Util

let intersect ray s =
  if (looseEq ray.direction.Y 0.)
  then []
  else [(-ray.origin.Y / ray.direction.Y, s)]
