module Transform

open System
open Matrix

let translation x y z =
  identity () |> replace 0 3 x |> replace 1 3 y |> replace 2 3 z

let scaling x y z =
  identity () |> replace 0 0 x |> replace 1 1 y |> replace 2 2 z

let rotationX rad =
  identity ()
  |> replace 1 1 (cos rad)
  |> replace 1 2 (- sin rad)
  |> replace 2 1 (sin rad)
  |> replace 2 2 (cos rad)