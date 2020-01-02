module Sphere

open System

open Ray
open Tuple

let uv (p: Tuple) =
  let d = p - (point 0. 0. 0.) |> normalize
  let u = 0.5f + MathF.Atan2 (d.Z, d.X) / (2.f * MathF.PI)
  let v = 0.5f - MathF.Asin d.Y / MathF.PI
  (u, v)

let intersect ray s =
  let sphereToRay = ray.origin - (point 0. 0. 0.)
  let a = dot ray.direction ray.direction
  let b = 2.f * (dot ray.direction sphereToRay)
  let c = (dot sphereToRay sphereToRay) - 1.f
  let discriminant = b ** 2.f - 4.f * a * c
  if (discriminant < 0.f)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2.f * a)
    let t2 = (-b + sqrt discriminant) / (2.f * a)
    [(t1, s); (t2, s)]
