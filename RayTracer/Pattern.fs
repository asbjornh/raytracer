module rec Pattern

open System

open Color
open Matrix
open Tuple

type IPattern =
  abstract member GetColor: Tuple -> Color
  abstract member Transform: Matrix

let patternAt (point: Tuple) (objectT: Matrix) (pattern: IPattern) =
  let localPoint = multiplyT (inverse objectT) point
  let patternPoint = multiplyT (inverse pattern.Transform) localPoint
  pattern.GetColor patternPoint

type StripePattern =
  { A: Color; B: Color; Transform: Matrix }
  interface IPattern with
    member this.Transform = this.Transform
    member this.GetColor point =
      if ((Math.Floor point.X) % 2. = 0.)
      then this.A
      else this.B

let stripePatternT a b t : StripePattern =
  { A = a; B = b; Transform = t }

let stripePattern a b = stripePatternT a b (identity ())


type GradientPattern =
  { A: Color; B: Color; Transform: Matrix }
  interface IPattern with
    member this.Transform = this.Transform
    member this.GetColor point =
      Color.blend this.A this.B (point.X - (Math.Floor point.X))

let gradientPatternT a b t : GradientPattern =
  { A = a; B = b; Transform = t }

let gradientPattern a b = gradientPatternT a b (identity ())


type RingPattern =
  { A: Color; B: Color; Transform: Matrix }
  interface IPattern with
    member this.Transform = this.Transform
    member this.GetColor point =
      let pointsSquared = sqrt (point.X ** 2. + point.Z ** 2.)
      if ((Math.Floor pointsSquared) % 2. = 0.)
      then this.A
      else this.B

let ringPatternT a b t : RingPattern =
  { A = a; B = b; Transform = t }

let ringPattern a b = ringPatternT a b (identity ())


type CheckersPattern =
  { A: Color; B: Color; Transform: Matrix }
  interface IPattern with
    member this.Transform = this.Transform
    member this.GetColor point =
      let sum = (Math.Floor point.X) + (Math.Floor point.Y) + (Math.Floor point.Z)
      if (sum % 2. = 0.) then this.A else this.B

let checkersPatternT a b t : CheckersPattern =
  { A = a; B = b; Transform = t }

let checkersPattern a b = checkersPatternT a b (identity ())
