module Color

open Util

type Color = float * float * float

let color r g b : Color = (r, g, b)

let combine op (a: Color) (b: Color) =
  let (x1, y1, z1) = a
  let (x2, y2, z2) = b
  (op x1 x2, op y1 y2, op z1 z2)

let map fn (c: Color) =
  let (x, y, z) = c
  (fn x, fn y, fn z)

let toList (r, g, b) = [r; g; b]

let toString transform sep =
  map (transform >> toString)
  >> toList
  >> String.concat sep

let epsilon = 0.00001
let valueEquals a b = abs (a - b) < epsilon

let equals a b = (combine valueEquals a b) |> function
  | (true, true, true) -> true
  | _ -> false

let add = combine (+)
let subtract = combine (-)
let multiply = combine (*)
let scale n = map ((*) n)
let divide n = map (flip (/) n)

let red = color 1. 0. 0.
let green = color 0. 1. 0.
let blue = color 0. 0. 1.
let white = color 1. 1. 1.
let black = color 0. 0. 0.
