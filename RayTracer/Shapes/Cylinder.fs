module rec Cylinder

open Ray
open Tuple
open Util

let cylMin = -1.
let cylMax = 1.

let normal (p: Tuple) =
  let dist = p.X ** 2. + p.Z ** 2.
  if (dist < 1. && p.Y >= cylMax - epsilon)
  then vector 0. 1. 0.
  else if (dist < 1. && p.Y <= cylMin + epsilon)
  then vector 0. -1. 0.
  else vector p.X 0. p.Z

let intersect ray s =
  List.concat [
    intersectOpen ray s
    intersectCaps ray s
  ]

let intersectOpen ray s =
  let a = ray.direction.X ** 2. + ray.direction.Z ** 2.
  if a < epsilon then []
  else
    let b =
      2. * ray.origin.X * ray.direction.X +
      2. * ray.origin.Z * ray.direction.Z
    let c = ray.origin.X ** 2. + ray.origin.Z ** 2. - 1.
    let discriminant = b ** 2. - 4. * a * c
    if discriminant < 0. then []
    else
      let _t0 = (-b - sqrt discriminant) / (2. * a)
      let _t1 = (-b + sqrt discriminant) / (2. * a)
      let (t0, t1) = (max _t0 _t1, min _t0 _t1)
      let y0 = ray.origin.Y + t0 * ray.direction.Y
      let y1 = ray.origin.Y + t1 * ray.direction.Y
      let first =
        if (cylMin < y0 && y0 < cylMax) then [(t0, s)] else []
      let second =
        if (cylMin < y1 && y1 < cylMax) then [(t1, s)] else []
      List.concat [first; second]

let checkCap ray t =
  let x = ray.origin.X + t * ray.direction.X
  let z = ray.origin.Z + t * ray.direction.Z
  (x ** 2. + z ** 2.) <= 1.

let intersectCaps ray cyl =
  if looseEq ray.direction.Y 0. then []
  else
    let t1 = (cylMin - ray.origin.Y) / ray.direction.Y
    let t2 = (cylMax - ray.origin.Y) / ray.direction.Y
    let first =
      if checkCap ray t1 then [(t1, cyl)] else []
    let second =
      if checkCap ray t2 then [(t2, cyl)] else []
    List.concat [first; second]
