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

let equals (a: Vector4) (b: Vector4) =
  looseEq32 a.X b.X &&
  looseEq32 a.Y b.Y &&
  looseEq32 a.Z b.Z &&
  looseEq32 a.W b.W

let map fn (a: Vector4) =
  Vector4 (fn a.X, fn a.Y, fn a.Z, fn a.W)

let fold fn init a =
  let l = a |> toList
  List.fold fn init l

let dot (a) (b) = Vector4.Dot (a, b)
let negate a = Vector4.Negate a
let magnitude (a: Vector4) =
  a |> map (fun x -> x * x) |> fold (+) 0.f |> sqrt
let normalize a = Vector4.Normalize a

let cross (a: Vector4) (b: Vector4) =
  let v = Vector3.Cross (toVec3 a, toVec3 b)
  vector32 v.X v.Y v.Z

let reflect (normal: Vector4) (v: Vector4) =
  v - (dot v normal * 2.f * normal)
let angle a b =
  (dot a b) / (magnitude a * magnitude b) |> MathF.Acos

let toVector (a: Vector4) = vector32 a.X a.Y a.Z
let toList (a: Vector4) = [a.X; a.Y; a.Z; a.W]
let to4 (a: Vector4) = (a.X, a.Y, a.Z, a.W)
let to3 (a: Vector4) = (a.X, a.Y, a.Z)
let toVec3 a = Vector3 (a.X, a.Y, a.Z)
let toPixel (a: Vector4) =
  let (x, y) = (a.X, a.Y)
  let fn = MathF.Round >> int
  (fn x, fn y)

let keyframe (start: Vector4) (finish: Vector4) frames =
  let diff = finish - start
  [0..frames-1]
  |> List.map (fun i ->
    let progress = float32 i / float32 (frames - 1)
    let (a1, b1, c1, d1) = start |> to4
    let (a2, b2, c2, _) = diff |> to4
    Vector4 (a1 + a2 * progress, b1 + b2 * progress, c1 + c2 * progress, d1)
  )
