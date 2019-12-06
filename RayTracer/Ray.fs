module Ray

open Tuple

type Ray = {
  origin: Tuple;
  direction: Tuple;
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position t ray =
  add ray.origin (multiply t ray.direction)
