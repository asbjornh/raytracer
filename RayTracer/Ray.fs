module Ray

open System.Numerics

type Ray = {
  origin: Vector4
  direction: Vector4
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position (t: float32) ray =
  ray.origin + t * ray.direction

let transform m r =
  ray (Transform.transform m r.origin) (Transform.transform m r.direction)
