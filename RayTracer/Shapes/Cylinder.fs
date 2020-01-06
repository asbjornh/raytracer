module rec Cylinder

open Ray
open Tuple
open Util

let cylMin = -1.f
let cylMax = 1.f

let normal (p: System.Numerics.Vector4) =
  let dist = p.X ** 2.f + p.Z ** 2.f
  if (dist < 1.f && p.Y >= cylMax - epsilon32)
  then vector32 0.f 1.f 0.f
  else if (dist < 1.f && p.Y <= cylMin + epsilon32)
  then vector32 0.f -1.f 0.f
  else vector32 p.X 0.f p.Z

let intersect ray s =
  List.concat [
    intersectOpen ray s
    intersectCaps ray s
  ]

let intersectOpen ray s =
  let a = ray.direction.X ** 2.f + ray.direction.Z ** 2.f
  if looseEq32 a 0.f then []
  else
    let b =
      2.f * ray.origin.X * ray.direction.X +
      2.f * ray.origin.Z * ray.direction.Z
    let c = ray.origin.X ** 2.f + ray.origin.Z ** 2.f - 1.f
    let discriminant = b ** 2.f - 4.f * a * c
    if discriminant < 0.f then []
    else
      let _t0 = (-b - sqrt discriminant) / (2.f * a)
      let _t1 = (-b + sqrt discriminant) / (2.f * a)
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
  (x ** 2.f + z ** 2.f) <= 1.f

let intersectCaps ray cyl =
  if looseEq32 ray.direction.Y 0.f then []
  else
    let t1 = (cylMin - ray.origin.Y) / ray.direction.Y
    let t2 = (cylMax - ray.origin.Y) / ray.direction.Y
    let first =
      if checkCap ray t1 then [(t1, cyl)] else []
    let second =
      if checkCap ray t2 then [(t2, cyl)] else []
    List.concat [first; second]
