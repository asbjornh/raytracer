module rec Cube

open Ray
open Tuple

let normal p =
  let (x, y, z, _) = Tuple.Map abs p
  let maxC = List.max [x; y; z]
  if maxC = x then vector p.X 0. 0.
  else if maxC = y then vector 0. p.Y 0.
  else vector 0. 0. p.Z

let intersect ray s =
  let (xtmin, xtmax) = checkAxis ray.origin.X ray.direction.X
  let (ytmin, ytmax) = checkAxis ray.origin.Y ray.direction.Y
  let (ztmin, ztmax) = checkAxis ray.origin.Z ray.direction.Z

  let tmin = List.max [xtmin; ytmin; ztmin]
  let tmax = List.min [xtmax; ytmax; ztmax]

  if tmin > tmax then []
  else [(tmin, s); (tmax, s)]

let checkAxis origin direction =
  let tmin = (-1. - origin) / direction
  let tmax = (1. - origin) / direction
  (min tmax tmin, max tmax tmin)
