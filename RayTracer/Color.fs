module Color

open Util

type Color (r, g, b) =
  member x.Return = (r, g, b)

  static member Combine fn (a: Color) (b: Color) =
    let (r1, g1, b1) = a.Return
    let (r2, g2, b2) = b.Return
    (fn r1 r2, fn g1 g2, fn b1 b2)

  static member CombineC fn a b = Color.Combine fn a b |> Color

  static member Map fn (c: Color) =
    let (r, g, b) = c.Return
    (fn r, fn g, fn b)

  static member MapC fn c = Color.Map fn c |> Color

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

let equals a b = (Color.Combine looseEq a b) |> function
  | (true, true, true) -> true
  | _ -> false

let add c = Color.CombineC (+) c
let subtract c = Color.CombineC (-) c
let multiply c = Color.CombineC (*) c
let scale n = Color.MapC ((*) n)
let divide n = Color.MapC (flip (/) n)
let lighten c = Color.CombineC max c
let invert c = subtract (color 1. 1. 1.) c
let screen a b =
  invert (multiply (invert a) (invert b))
let mix a b amount =
  subtract b a |> scale amount |> add a

type BlendingMode =
  | Add
  | Subtract
  | Multiply
  | Lighten
  | Screen
  | Normal

let blend = function
  | Add -> add
  | Subtract -> subtract
  | Multiply -> multiply
  | Lighten -> lighten
  | Screen -> screen
  | Normal -> always

let red = color 1. 0.1 0.2
let green = color 0. 1. 0.
let blue = color 0.2 0.4 1.
let white = color 1. 1. 1.
let black = color 0. 0. 0.
let yellow = color 1. 0.9 0.
let darkGray = color 0.1 0.1 0.1
let pink = color 1. 0.3 0.9
let cyan = color 0.2 1. 1.
let gold = color 0.83 0.69 0.22
