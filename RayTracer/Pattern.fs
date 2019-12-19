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
    if ((Math.Floor point.X) % 2. = 0.)
    then a else b
  | Checkers ->
    let sum = (Math.Floor point.X) + (Math.Floor point.Y) + (Math.Floor point.Z)
    if (sum % 2. = 0.) then a else b
  | Ring ->
    let pointsSquared = sqrt (point.X ** 2. + point.Z ** 2.)
    if ((Math.Floor pointsSquared) % 2. = 0.)
    then a else b


let gradientAt a b sharpness (point: Tuple) =
  let amount = point.X - Math.Floor point.X
  if (sharpness = 0.) then
    Color.blend a b amount
  else
    let p1 = 0. - sharpness
    let p2 = 1. + sharpness
    let mapped = cubicBezier p1 p2 amount |> clamp 0. 1.
    Color.blend a b mapped
