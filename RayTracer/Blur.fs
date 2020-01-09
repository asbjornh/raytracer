module Blur

open System

open Color
open Util

let boxBlur size (image: Color[][]) =
  image
  |> map2di (fun x y c ->
    image
    |> subGrid x y size |> Array.concat
    |> flip appendTo c
    |> Array.toList
    |> Color.average
  )

let medianFilter size (image: Color[][]) =
  let medianIndex =
    (float size * float size) / 2. |> Math.Floor |> int
  image
  |> map2di (fun x y c ->
    image
    |> subGrid x y size |> Array.concat
    |> Array.sortBy (fun c ->
      let (r, g, b) = c.Return
      r + g + b
    )
    |> Array.item medianIndex
  )
