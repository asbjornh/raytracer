module Poly

open Tuple

type Poly = {
  points: Tuple list
  e1: Tuple
  e2: Tuple
  normal: Tuple
}

let make p1 p2 p3 =
  let points = [p1; p2; p3]
  let e1 = p2 - p1
  let e2 = p3 - p1
  let normal = cross e1 e2 |> normalize
  { points = points; e1 = e1; e2 = e2; normal = normal }

let bounds p =
  let (xs, ys, zs) =
    p.points |> List.map toXYZ |> List.unzip3
  ( (List.min xs, List.max xs),
    (List.min ys, List.max ys),
    (List.min zs, List.max zs) )

let intersect (p: Poly) r =
  [0.]
