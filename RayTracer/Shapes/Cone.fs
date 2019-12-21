module rec Cone

open Ray
open Tuple
open Util

let normal minY maxY (p: Tuple) =
  let dist = p.X ** 2. + p.Z ** 2.
  if (dist < 1. && p.Y >= maxY - epsilon)
  then vector 0. 1. 0.
  else if (dist < 1. && p.Y <= minY + epsilon)
  then vector 0. -1. 0.
  else
    let _y = sqrt <| p.X ** 2. + p.Z ** 2.
    let y = if p.Y > 0. then -_y else _y
    vector p.X y p.Z

let intersect minY maxY ray s =
  List.concat [
    intersectCone minY maxY ray s
    intersectCaps minY maxY ray s
  ]

let intersectCone minY maxY ray s =
  let (oX, oY, oZ, _) = ray.origin.Return
  let (dX, dY, dZ, _) = ray.direction.Return
  let a = (dX ** 2.) - (dY ** 2.) + (dZ ** 2.)
  let b = (2. * oX * dX) - (2. * oY * dY) + (2. * oZ * dZ)
  let c = (oX ** 2.) - (oY ** 2.) + (oZ ** 2.)

  if looseEq a 0. && looseEq b 0. then []
  else if looseEq a 0. then [(-c / (2. * b), s)]
  else
    let discriminant = b ** 2. - 4. * a * c
    if discriminant < 0. then []
    else
      let _t0 = (-b - sqrt discriminant) / (2. * a)
      let _t1 = (-b + sqrt discriminant) / (2. * a)
      let (t0, t1) = (max _t0 _t1, min _t0 _t1)
      let y0 = ray.origin.Y + t0 * ray.direction.Y
      let y1 = ray.origin.Y + t1 * ray.direction.Y
      let first =
        if (minY < y0 && y0 < maxY) then [(t0, s)] else []
      let second =
        if (minY < y1 && y1 < maxY) then [(t1, s)] else []
      List.concat [first; second]

let checkCap ray t r =
  let x = ray.origin.X + t * ray.direction.X
  let z = ray.origin.Z + t * ray.direction.Z
  (x ** 2. + z ** 2.) <= r ** 2.

let intersectCaps minY maxY ray cyl =
  if looseEq ray.direction.Y 0. then []
  else
    let t1 = (minY - ray.origin.Y) / ray.direction.Y
    let t2 = (maxY - ray.origin.Y) / ray.direction.Y
    let first =
      if checkCap ray t1 minY then [(t1, cyl)] else []
    let second =
      if checkCap ray t2 maxY then [(t2, cyl)] else []
    List.concat [first; second]
