module rec Pattern

open System

open Matrix
open Tuple
open Util

type PatternType =
  | Stripes
  | Checkers
  | Ring
  
let patternPoint objectT patternT point =
  multiplyT (inverse objectT) point
  |> multiplyT (inverse patternT)

let patternAt a b (pattern: PatternType) (point: Tuple) =
  match pattern with
  | Stripes ->
    if ((MathF.Floor point.X) % 2.f = 0.f)
    then a else b
  | Checkers ->
    let sum = (MathF.Floor point.X) + (MathF.Floor point.Y) + (MathF.Floor point.Z)
    if (sum % 2.f = 0.f) then a else b
  | Ring ->
    let pointsSquared = sqrt (point.X ** 2.f + point.Z ** 2.f)
    if ((MathF.Floor pointsSquared) % 2.f = 0.f)
    then a else b


let gradientAt a b sharpness (point: Tuple) =
  let amount = point.X - MathF.Floor point.X |> float
  if (sharpness = 0.) then
    Color.mix a b amount
  else
    let p1 = 0. - sharpness
    let p2 = 1. + sharpness
    let mapped = cubicBezier p1 p2 amount |> clamp 0. 1.
    Color.mix a b mapped
