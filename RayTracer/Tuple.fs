module Tuple

open Util

type Tuple(a, b, c, d) =
  member x.X = a
  member x.Y = b
  member x.Z = c
  member x.W = d
  member x.Return = (x.X, x.Y, x.Z, x.W)

  static member Map fn (a: Tuple) =
    let (x, y, z, w) = a.Return
    (fn x, fn y, fn z, fn w)
    
  static member MapT fn (a: Tuple) =
    Tuple.Map fn a |> Tuple

  static member Combine fn (a: Tuple) (b: Tuple) =
    let (x1, y1, z1, w1) = a.Return
    let (x2, y2, z2, w2) = b.Return
    (fn x1 x2, fn y1 y2, fn z1 z2, fn w1 w2)

  static member CombineT fn a b =
    Tuple.Combine fn a b |> Tuple

  static member Fold fn init (a: Tuple) =
    let (x, y, z, w) = a.Return
    fn x init |> fn y |> fn z |> fn w

  static member (*) (a, b: Tuple) =
    Tuple.MapT ((*) a) b

  static member (/) (a, b: Tuple) =
    Tuple.MapT (flip (/) a) b

  static member (/.) (a: Tuple, b: Tuple) =
    Tuple.CombineT (*) a b |> Tuple.Fold (+) 0.

  static member (+) (a: Tuple, b: Tuple) =
    Tuple.CombineT (+) a b

  static member (-) (a: Tuple, b: Tuple) =
    Tuple.CombineT (-) a b

  override x.GetHashCode () = x.GetHashCode ()
  override x.Equals a =
    match a with
      | :? Tuple as a ->
        let (x, y, z, w) = Tuple.Combine looseEq a x
        x && y && z && w
      | _ -> false

let point x y z = Tuple(x, y, z, 1.0)
let vector x y z = Tuple(x, y, z, 0.0)

let zero = vector 0. 0. 0.
let negate (a: Tuple) = Tuple.CombineT (-) zero a
let magnitude (a: Tuple) =
  Tuple.MapT (pow 2.0) a |> Tuple.Fold (+) 0. |> sqrt
let normalize (a: Tuple) =
  (magnitude a) / a

let cross (a: Tuple) (b: Tuple) =
  let (x1, y1, z1, _) = a.Return
  let (x2, y2, z2, _) = b.Return
  let x = y1 * z2 - z1 * y2
  let y = z1 * x2 - x1 * z2
  let z = x1 * y2 - y1 * x2
  vector x y z

let toMatrix (a: Tuple) =
  let (x, y, z, w) = a.Return
  [| [|x|]; [|y|]; [|z|]; [|w|] |]

let to2d (x, y, _, _) = (x, y)