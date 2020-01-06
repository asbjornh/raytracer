module Transform

open System.Numerics

open Matrix
open Tuple

let transform (t: Matrix4x4) (a: Vector4) =
  Vector4.Transform (a, t)
let translate x y z = Matrix4x4.CreateTranslation <| Vector3 (x, y, z)
let translateX x = translate x 0.f 0.f
let translateY y = translate 0.f y 0.f
let translateZ z = translate 0.f 0.f z

let scale x y z = Matrix4x4.CreateScale (x, y, z)
let scaleX x = scale x 1.f 1.f
let scaleY y = scale 1.f y 1.f
let scaleZ z = scale 1.f 1.f z
let uniformScale (s: float32) = Matrix4x4.CreateScale s

let rotateX rad = Matrix4x4.CreateRotationX rad
let rotateY rad = Matrix4x4.CreateRotationY rad
let rotateZ rad = Matrix4x4.CreateRotationZ rad



let lookAt (position: Vector4) (target: Vector4) (up: Vector4) =
  let t =
    Matrix4x4.CreateLookAt
    <| (position |> toVec3, target |> toVec3, up |> toVec3)
  let (_, _, r, _) = Matrix4x4.Decompose t
  Matrix4x4.CreateFromQuaternion r

let rotateAlign (fromV: Vector4) (toV: Vector4) =
  // NOTE: https://gist.github.com/kevinmoran/b45980723e53edeb8a5a43c49f134724
  let f = normalize fromV
  let t = normalize toV
  let cosA = dot f t
  if (Tuple.equals f t)
    then identity
  else if (cosA = -1.f)
    then failwith "'from' and 'To' can't point in opposite directions"
  else
    let a = cross f t
    let k = 1.f / (1.f + cosA)
    Matrix4x4 ( 
      (a.X * a.X * k) + cosA,
      (a.Y * a.X * k) - a.Z,
      (a.Z * a.X * k) + a.Y,
      0.0f,

      (a.X * a.Y * k) + a.Z,
      (a.Y * a.Y * k) + cosA,
      (a.Z * a.Y * k) - a.X,
      0.0f,

      (a.X * a.Z * k) - a.Y,
      (a.Y * a.Z * k) + a.X,
      (a.Z * a.Z * k) + cosA, 
      0.0f,

      0.0f, 0.0f, 0.0f, 1.0f
    );

let chain = List.reduce multiply

let viewTransform (from: Vector4) (To: Vector4) up =
  let f = from |> toVec3
  let t = To |> toVec3
  let u = up |> toVec3
  Matrix4x4.CreateLookAt (f, t, u)
