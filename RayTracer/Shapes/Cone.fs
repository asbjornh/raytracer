module rec Cone

open Ray
open Tuple
open Util

let normal minY maxY (p: System.Numerics.Vector4) =
  let dist = p.X ** 2.f + p.Z ** 2.f
  if (dist < 1.f && p.Y >= maxY - epsilon32)
  then vector 0. 1. 0.
  else if (dist < 1.f && p.Y <= minY + epsilon32)
  then vector 0. -1. 0.
  else
    let _y = sqrt <| p.X ** 2.f + p.Z ** 2.f
    let y = if p.Y > 0.f then -_y else _y
    vector32 p.X y p.Z

let intersect minY maxY ray s =
  List.concat [
    intersectCone minY maxY ray s
    intersectCaps minY maxY ray s
  ]

let intersectCone minY maxY ray s =
  let (oX, oY, oZ) = toXYZ ray.origin
  let (dX, dY, dZ) = toXYZ ray.direction
  let a = (dX ** 2.f) - (dY ** 2.f) + (dZ ** 2.f)
  let b = (2.f * oX * dX) - (2.f * oY * dY) + (2.f * oZ * dZ)
  let c = (oX ** 2.f) - (oY ** 2.f) + (oZ ** 2.f)

  if looseEq32 a 0.f && looseEq32 b 0.f then []
  else if looseEq32 a 0.f then [(-c / (2.f * b), s)]
  else
    let discriminant = b ** 2.f - 4.f * a * c
    if discriminant < 0.f then []
    else
      let _t0 = (-b - sqrt discriminant) / (2.f * a)
      let _t1 = (-b + sqrt discriminant) / (2.f * a)
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
  (x ** 2.f + z ** 2.f) <= r ** 2.f

let intersectCaps minY maxY ray cyl =
  if looseEq32 ray.direction.Y 0.f then []
  else
    let t1 = (minY - ray.origin.Y) / ray.direction.Y
    let t2 = (maxY - ray.origin.Y) / ray.direction.Y
    let first =
      if checkCap ray t1 minY then [(t1, cyl)] else []
    let second =
      if checkCap ray t2 maxY then [(t2, cyl)] else []
    List.concat [first; second]
