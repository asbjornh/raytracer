module rec Tuple

open System
open System.Numerics

open Util

type Tuple (a, b, c, d) =
  member x.Vec = Vector4 (a, b, c, d)
  member x.X = x.Vec.X
  member x.Y = x.Vec.Y
  member x.Z = x.Vec.Z
  member x.W = x.Vec.W
  member x.Return = (x.Vec.X, x.Vec.Y, x.Vec.Z, x.Vec.W)

  static member Map fn (a: Tuple) =
    let (x, y, z, w) = a.Return
    (fn x, fn y, fn z, fn w)
    
  static member MapT fn (a: Tuple) =
    Tuple.Map fn a |> Tuple

  static member Map2 fn (a: Tuple) (b: Tuple) =
    let (x1, y1, z1, w1) = a.Return
    let (x2, y2, z2, w2) = b.Return
    (fn x1 x2, fn y1 y2, fn z1 z2, fn w1 w2)

  static member Map2T fn a b =
    Tuple.Map2 fn a b |> Tuple

  static member Fold fn init (a: Tuple) =
    let (x, y, z, w) = a.Return
    fn x init |> fn y |> fn z |> fn w

  static member (*) (a, b: Tuple) =
    Tuple.MapT ((*) a) b

  static member (/) (a, b: Tuple) =
    Tuple.MapT (flip (/) a) b

  static member (+) (a: Tuple, b: Tuple) =
    Tuple.Map2T (+) a b

  static member (-) (a: Tuple, b: Tuple) =
    Tuple.Map2T (-) a b

  override x.ToString () = x.Return.ToString ()
  override x.GetHashCode () = x.Return.GetHashCode ()
  override x.Equals a =
    match a with
      | :? Tuple as a ->
        let (x, y, z, w) = Tuple.Map2 looseEq32 a x
        x && y && z && w
      | _ -> false

let point32 x y z = Tuple (x, y, z, 1.0f)
let point (x: float) (y: float) (z: float) =
  point32 (float32 x) (float32 y) (float32 z)
let vector32 x y z = Tuple (x, y, z, 0.0f)
let vector (x: float) (y: float) (z: float) =
  vector32 (float32 x) (float32 y) (float32 z)
let fromVec (a: Vector4) =
  Tuple (a.X, a.Y, a.Z, a.W)

let dot (a: Tuple) (b: Tuple) =
  Vector4.Dot (a.Vec, b.Vec)
let negate (a: Tuple) = Vector4.Negate a.Vec |> fromVec
let magnitude (a: Tuple) =
  Tuple.MapT (fun x -> x * x) a |> Tuple.Fold (+) 0.f |> sqrt
let normalize (a: Tuple) = Vector4.Normalize a.Vec |> fromVec

let cross (a: Tuple) (b: Tuple) =
  let a3 = a |> toXYZ |> Vector3
  let b3 = b |> toXYZ |> Vector3
  let v = Vector3.Cross (a3, b3)
  Tuple (v.X, v.Y, v.Z, 0.0f)

let reflect (normal: Tuple) (v: Tuple) =
  v - (dot v normal * 2.f * normal)
let angle a b =
  (dot a b) / (magnitude a * magnitude b) |> MathF.Acos

let toVector (a: Tuple) =
  let (x, y, z, _) = a.Return
  Tuple (x, y, z, 0.f)
let toArray (a: Tuple) =
  let (x, y, z, w) = a.Return
  [| [|x|]; [|y|]; [|z|]; [|w|]; |]

let toXYZ (a: Tuple) : (float32 * float32 * float32) =
  (a.Vec.X, a.Vec.Y, a.Vec.Z)
let toPixel a =
  a |> Tuple.Map (MathF.Round >> int)
  |> fun (x, y, _, _) -> (x, y)

let keyframe (start: Tuple) (finish: Tuple) frames =
  let diff = finish - start
  [0..frames-1]
  |> List.map (fun i ->
    let progress = float32 i / float32 (frames - 1)
    let (a1, b1, c1, d1) = start.Return
    let (a2, b2, c2, _) = diff.Return
    Tuple (a1 + a2 * progress, b1 + b2 * progress, c1 + c2 * progress, d1)
  )
