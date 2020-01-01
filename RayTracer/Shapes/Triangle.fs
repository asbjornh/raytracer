module Triangle

open Ray
open Tuple
open Util

type Triangle = {
  p1: Tuple
  p2: Tuple
  p3: Tuple
  e1: Tuple
  e2: Tuple
  normal: Tuple
}

let make p1 p2 p3 =
  let e1 = p2 - p1
  let e2 = p3 - p1
  let normal = cross e1 e2 |> normalize
  { p1 = p1; p2 = p2; p3 = p3; e1 = e1; e2 = e2; normal = normal }

let bounds p =
  let (xs, ys, zs) =
    [p.p1; p.p2; p.p3] |> List.map toXYZ |> List.unzip3
  ( (List.min xs, List.max xs),
    (List.min ys, List.max ys),
    (List.min zs, List.max zs) )

let intersect t ray =
  let dirCrossE2 = cross ray.direction t.e2
  let determinant = dot dirCrossE2 t.e1
  if (looseEq determinant 0.) then []
  else
    let f = 1. / determinant
    let p1ToOrigin = ray.origin - t.p1
    let u = f * dot p1ToOrigin dirCrossE2
    if (u < 0. || u > 1.) then []
    else
      let originCrossE1 = cross p1ToOrigin t.e1
      let v = f * dot ray.direction originCrossE1
      if (v < 0. || u + v > 1.) then []
      else [f * dot t.e2 originCrossE1]
