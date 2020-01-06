module rec Tuple

open System
open System.Numerics

open Util

let point32 x y z = Vector4 (x, y, z, 1.0f)
let point (x: float) (y: float) (z: float) =
  point32 (float32 x) (float32 y) (float32 z)

let vector32 x y z = Vector4 (x, y, z, 0.0f)
let vector (x: float) (y: float) (z: float) =
  vector32 (float32 x) (float32 y) (float32 z)

let equals a b =
  let (x, y, z, w) = map2 looseEq32 a b
  x && y && z && w

let mapTo4 fn a =
  let (x, y, z, w) = a |> to4
  (fn x, fn y, fn z, fn w)

let map fn a =
  let (x, y, z, w) = a |> to4
  Vector4 (fn x, fn y, fn z, fn w)

let map2 fn a b =
  let (x1, y1, z1, w1) = a |> to4
  let (x2, y2, z2, w2) = b |> to4
  (fn x1 x2, fn y1 y2, fn z1 z2, fn w1 w2)

let fold fn init a =
  let l = a |> toList
  List.fold fn init l

let dot (a) (b) = Vector4.Dot (a, b)
let negate a = Vector4.Negate a
let magnitude (a: Vector4) =
  a |> map (fun x -> x * x) |> fold (+) 0.f |> sqrt
let normalize a = Vector4.Normalize a

let cross (a: Vector4) (b: Vector4) =
  let a3 = a |> toVec3
  let b3 = b |> toVec3
  let v = Vector3.Cross (a3, b3)
  vector32 v.X v.Y v.Z

let reflect (normal: Vector4) (v: Vector4) =
  v - (dot v normal * 2.f * normal)
let angle a b =
  (dot a b) / (magnitude a * magnitude b) |> MathF.Acos

let toVector (a: Vector4) =
  let (x, y, z) = a |> toXYZ
  vector32 x y z

let toList (a: Vector4) =
  let (x, y, z, w) = a |> to4
  [x; y; z; w]

let to4 (a: Vector4) = (a.X, a.Y, a.Z, a.W)
let toXYZ (a: Vector4) = (a.X, a.Y, a.Z)
let toVec3 t = t |> toXYZ |> Vector3
let toPixel a =
  a |> mapTo4 (MathF.Round >> int)
  |> fun (x, y, _, _) -> (x, y)

let keyframe (start: Vector4) (finish: Vector4) frames =
  let diff = finish - start
  [0..frames-1]
  |> List.map (fun i ->
    let progress = float32 i / float32 (frames - 1)
    let (a1, b1, c1, d1) = start |> to4
    let (a2, b2, c2, _) = diff |> to4
    Vector4 (a1 + a2 * progress, b1 + b2 * progress, c1 + c2 * progress, d1)
  )
