module Ray

open Tuple
open Matrix

type Ray = {
  origin: Tuple;
  direction: Tuple;
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position t ray =
  ray.origin + t * ray.direction

let transform m r =
  ray (multiplyT m r.origin) (multiplyT m r.direction)
