module Pattern

open System

open Color
open Tuple

type StripePattern = {
  a: Color
  b: Color
}

let stripePattern a b : StripePattern =
  { a = a; b = b }

let stripeAt (point: Tuple) pattern =
  if ((Math.Floor point.X) % 2. = 0.)
  then pattern.a
  else pattern.b
