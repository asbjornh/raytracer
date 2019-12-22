module Sphere

open System

open Ray
open Tuple

let uv (p: Tuple) =
  let d = p - (point 0. 0. 0.) |> normalize
  let u = 0.5 + Math.Atan2 (d.Z, d.X) / (2. * Math.PI)
  let v = 0.5 - Math.Asin d.Y / Math.PI
  (u, v)

let intersect ray s =
  let sphereToRay = ray.origin - (point 0. 0. 0.)
  let a = dot ray.direction ray.direction
  let b = 2. * (dot ray.direction sphereToRay)
  let c = (dot sphereToRay sphereToRay) - 1.
  let discriminant = b ** 2. - 4. * a * c
  if (discriminant < 0.)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2. * a)
    let t2 = (-b + sqrt discriminant) / (2. * a)
    [(t1, s); (t2, s)]
