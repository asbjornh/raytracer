module Ray

open Util
open Tuple

type Ray = {
  origin: Tuple;
  direction: Tuple;
}

let ray origin direction =
  { origin = origin; direction = direction; }

let position t ray =
  ray.origin + t * ray.direction
