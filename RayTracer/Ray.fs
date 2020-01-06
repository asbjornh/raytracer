module Ray

open System.Numerics

open Matrix

type Ray = {
  origin: Vector4
  direction: Vector4
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position (t: float32) ray =
  ray.origin + t * ray.direction

let transform m r =
  ray (multiplyT m r.origin) (multiplyT m r.direction)
