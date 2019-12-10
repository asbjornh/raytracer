module Sphere

open Material
open Matrix
open Tuple

type Sphere = {
  transform: Matrix;
  mutable material: Material;
}

let sphere () = {
  transform = identity ()
  material = material ()
}

let transform t s = { s with transform = t }

let normal (p: Tuple) s =
  let invT = inverse s.transform
  let objectN = (multiplyT invT p) - (point 0. 0. 0.)
  let worldN = multiplyT (transpose invT) objectN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)
