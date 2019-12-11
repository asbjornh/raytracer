module Transform

open System
open Matrix
open Tuple

let translation x y z =
  identity () |> set 0 3 x |> set 1 3 y |> set 2 3 z

let scaling x y z =
  identity () |> set 0 0 x |> set 1 1 y |> set 2 2 z

let rotationX rad =
  identity ()
  |> set 1 1 (cos rad) |> set 1 2 (- sin rad)
  |> set 2 1 (sin rad) |> set 2 2 (cos rad)

let rotationY rad =
  identity ()
  |> set 0 0 (cos rad) |> set 0 2 (sin rad)
  |> set 2 0 (-sin rad) |> set 2 2 (cos rad)

let rotationZ rad =
  identity ()
  |> set 0 0 (cos rad) |> set 0 1 (-sin rad)
  |> set 1 0 (sin rad) |> set 1 1 (cos rad)

let shearing xy xz yx yz zx zy =
  identity ()
  |> set 0 1 xy |> set 0 2 xz
  |> set 1 0 yx |> set 1 2 yz
  |> set 2 0 zx |> set 2 1 zy

let translate x y z = translation x y z |> multiply
let translateX x = translation x 0. 0. |> multiply
let translateY y = translation 0. y 0. |> multiply
let translateZ z = translation 0. 0. z |> multiply
let scale x y z = scaling x y z |> multiply
let uniformScale s = scaling s s s |> multiply
let rotateX rad = rotationX rad |> multiply
let rotateY rad = rotationY rad |> multiply
let rotateZ rad = rotationZ rad |> multiply
let shear xy xz yx yz zx zy =
  shearing xy xz yx yz zx zy |> multiply

let chain fns = List.rev fns |> List.reduce (>>) <| identity ()

let viewTransform (from: Tuple) To up =
  let forward = To - from |> normalize
  let left = cross forward (normalize up)
  let trueUp = cross left forward
  let orientation = matrix [
    [ left.X ; left.Y ; left.Z ; 0. ]
    [ trueUp.X ; trueUp.Y ; trueUp.Z; 0. ]
    [ -forward.X ; -forward.Y ; -forward.Z ; 0. ]
    [ 0. ; 0. ; 0. ; 1. ]
  ]
  orientation * (translation -from.X -from.Y -from.Z)
