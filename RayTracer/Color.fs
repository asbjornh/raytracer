module Color

open Util

type Color = float * float * float

let color r g b = (r, g, b)

let combine op (a: Color) (b: Color) =
  let (x1, y1, z1) = a
  let (x2, y2, z2) = b
  (op x1 x2, op y1 y2, op z1 z2)

let map fn t =
  let (x, y, z) = t
  Color (fn x, fn y, fn z)

let epsilon = 0.00001
let valueEquals a b = abs (a - b) < epsilon

let equals a b = (combine valueEquals a b) |> function
  | (true, true, true) -> true
  | _ -> false


type Add = Color -> Color -> Color
let (add: Add) = combine (+)

type Subtract = Color -> Color -> Color
let (subtract: Subtract) = combine (-)

type Multiply = Color -> Color -> Color
let (multiply: Multiply) = combine (*)

let scale n = map ((*) n)

let divide n = map (flip (/) n)