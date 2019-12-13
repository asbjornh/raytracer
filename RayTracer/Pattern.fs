module rec Pattern

open System

open Color
open Tuple

type IPattern =
  abstract member GetColor: Tuple -> Color

let colorAt (point: Tuple) (pattern: IPattern) =
  pattern.GetColor point

type StripePattern =
  {
    a: Color
    b: Color
  }
  interface IPattern with
    member this.GetColor point =
      if ((Math.Floor point.X) % 2. = 0.)
      then this.a
      else this.b

let stripePattern a b : StripePattern =
  { a = a; b = b }
