module Color

open Util

type Color (r, g, b) =
  member x.Return = (r, g, b)

  static member Combine fn (a: Color) (b: Color) =
    let (r1, g1, b1) = a.Return
    let (r2, g2, b2) = b.Return
    (fn r1 r2, fn g1 g2, fn b1 b2)

  static member Map fn (c: Color) =
    let (r, g, b) = c.Return
    (fn r, fn g, fn b)

  override x.ToString () = x.Return.ToString ()
  override x.GetHashCode () = x.Return.GetHashCode ()
  override x.Equals a =
    match a with
    | :? Color as c ->
      let (r, g, b) = Color.Combine looseEq c x
      r && g && b
    | _ -> false

let color r g b = Color (r, g, b)

let toList (r, g, b) = [r; g; b]

let toString transform sep =
  Color.Map (transform >> toString)
  >> toList
  >> String.concat sep

let epsilon = 0.00001
let valueEquals a b = abs (a - b) < epsilon

let equals a b = (Color.Combine valueEquals a b) |> function
  | (true, true, true) -> true
  | _ -> false

let add (a: Color) = Color.Combine (+) a >> Color
let subtract (a: Color) = Color.Combine (-) a >> Color
let multiply (a: Color) = Color.Combine (*) a >> Color
let scale n = Color.Map ((*) n) >> Color
let divide n = Color.Map (flip (/) n) >> Color

let red = color 1. 0. 0.
let green = color 0. 1. 0.
let blue = color 0. 0. 1.
let white = color 1. 1. 1.
let black = color 0. 0. 0.
