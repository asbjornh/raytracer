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

type SmoothTriangle = {
  p1: Tuple
  p2: Tuple
  p3: Tuple
  e1: Tuple
  e2: Tuple
  n1: Tuple
  n2: Tuple
  n3: Tuple
}

let make p1 p2 p3 =
  let e1 = p2 - p1
  let e2 = p3 - p1
  let normal = cross e1 e2 |> normalize
  { p1=p1; p2=p2; p3=p3; e1=e1; e2=e2; normal=normal }

let smoothMake p1 p2 p3 n1 n2 n3 =
  let e1 = p2 - p1
  let e2 = p3 - p1
  { p1=p1; p2=p2; p3=p3; e1=e1; e2=e2; n1=n1; n2=n2; n3=n3 }

let boundsRaw p1 p2 p3 =
  let (xs, ys, zs) =
    [p1; p2; p3] |> List.map toXYZ |> List.unzip3
  ( (List.min xs, List.max xs),
    (List.min ys, List.max ys),
    (List.min zs, List.max zs) )

let bounds (t: Triangle) =
  boundsRaw t.p1 t.p2 t.p3

let boundsSmooth (t: SmoothTriangle) =
  boundsRaw t.p1 t.p2 t.p3

let intersectRaw p1 e1 e2 ray =
  let dirCrossE2 = cross ray.direction e2
  let determinant = dot dirCrossE2 e1
  if (looseEq32 determinant 0.f) then []
  else
    let f = 1.f / determinant
    let p1ToOrigin = ray.origin - p1
    let u = f * dot p1ToOrigin dirCrossE2
    if (u < 0.f || u > 1.f) then []
    else
      let originCrossE1 = cross p1ToOrigin e1
      let v = f * dot ray.direction originCrossE1
      if (v < 0.f || u + v > 1.f) then []
      else [(f * dot e2 originCrossE1, (u, v))]

let intersect (t: Triangle) ray =
  intersectRaw t.p1 t.e1 t.e2 ray |> List.map fst

let intersectSmooth (t: SmoothTriangle) ray =
  intersectRaw t.p1 t.e1 t.e2 ray |> List.map fst

let localUV (t: Triangle) ray =
  intersectRaw t.p1 t.e1 t.e2 ray |> List.map snd |> headToOption

let localUVSmooth (t: SmoothTriangle) ray =
  intersectRaw t.p1 t.e1 t.e2 ray |> List.map snd |> headToOption

let normalAtSmooth t (u,v) =
  u * t.n2 +
  v * t.n3 +
  (1.f - u - v) * t.n1
