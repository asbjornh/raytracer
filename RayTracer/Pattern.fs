module rec Pattern

open System

open Color
open Matrix
open Tuple

type IPattern =
  abstract member GetColor: Tuple -> Color
  abstract member Transform: Matrix

let colorAt (point: Tuple) (objectT: Matrix) (pattern: IPattern) =
  let localPoint = multiplyT (inverse objectT) point
  let patternPoint = multiplyT (inverse pattern.Transform) localPoint
  pattern.GetColor patternPoint

type StripePattern =
  {
    A: Color
    B: Color
    Transform: Matrix
  }
  interface IPattern with
    member this.Transform = this.Transform
    member this.GetColor point =
      if ((Math.Floor point.X) % 2. = 0.)
      then this.A
      else this.B

let stripePatternT a b t : StripePattern =
  { A = a; B = b; Transform = t }

let stripePattern a b = stripePatternT a b (identity ())
