module rec Pattern

open System

open Matrix
open Tuple

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

let gradientAt a b (point: Tuple) =
  Color.blend a b (point.X - (Math.Floor point.X))
