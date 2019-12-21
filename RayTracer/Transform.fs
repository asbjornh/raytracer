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


let rotateAlignment (from: Tuple) (To: Tuple) =
  // NOTE: https://gist.github.com/kevinmoran/b45980723e53edeb8a5a43c49f134724
  let f = normalize from
  let t = normalize To
  let cosA = dot f t
  if (f = t)
    then identity ()
  else if (cosA = -1.)
    then failwith "'from' and 'To' can't point in opposite directions"
  else
    let a = cross f t
    let k = 1. / (1. + cosA)
    matrix [
      [ (a.X * a.X * k) + cosA;
        (a.Y * a.X * k) - a.Z;
        (a.Z * a.X * k) + a.Y;
        0.0 ]
      [ (a.X * a.Y * k) + a.Z;
        (a.Y * a.Y * k) + cosA;
        (a.Z * a.Y * k) - a.X;
        0.0 ]
      [ (a.X * a.Z * k) - a.Y;
        (a.Y * a.Z * k) + a.X;
        (a.Z * a.Z * k) + cosA 
        0.0 ]
      [ 0.0 ; 0.0 ; 0.0 ; 1.0 ]
    ];

let translate x y z = translation x y z |> multiply
let translateX x = translation x 0. 0. |> multiply
let translateY y = translation 0. y 0. |> multiply
let translateZ z = translation 0. 0. z |> multiply
let scale x y z = scaling x y z |> multiply
let scaleX x = scaling x 1. 1. |> multiply
let scaleY y = scaling 1. y 1. |> multiply
let scaleZ z = scaling 1. 1. z |> multiply
let uniformScale s = scaling s s s |> multiply
let rotateX rad = rotationX rad |> multiply
let rotateY rad = rotationY rad |> multiply
let rotateZ rad = rotationZ rad |> multiply
let rotateAlign f t = rotateAlignment f t |> multiply
let shear xy xz yx yz zx zy =
  shearing xy xz yx yz zx zy |> multiply

let chain fns = List.rev fns |> List.reduce (>>) <| identity ()

let viewTransform (from: Tuple) (To: Tuple) up =
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
