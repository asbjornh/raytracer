module rec Cube

open Ray
open Tuple

let normal p =
  let (x, y, z, _) = mapTo4 abs p
  let maxC = List.max [x; y; z]
  if maxC = x then vector32 p.X 0.f 0.f
  else if maxC = y then vector32 0.f p.Y 0.f
  else vector32 0.f 0.f p.Z


let intersectBox (xMin, xMax) (yMin, yMax) (zMin, zMax) ray s =
  let (xtmin, xtmax) = checkAxis xMin xMax ray.origin.X ray.direction.X
  let (ytmin, ytmax) = checkAxis yMin yMax ray.origin.Y ray.direction.Y
  let (ztmin, ztmax) = checkAxis zMin zMax ray.origin.Z ray.direction.Z

  let tmin = List.max [xtmin; ytmin; ztmin]
  let tmax = List.min [xtmax; ytmax; ztmax]

  if tmin > tmax then []
  else [(tmin, s); (tmax, s)]

let intersect ray =
  intersectBox (-1.f, 1.f) (-1.f, 1.f) (-1.f, 1.f) ray

let checkAxis minimum maximum origin direction =
  let tmin = (minimum - origin) / direction
  let tmax = (maximum - origin) / direction
  (min tmax tmin, max tmax tmin)
