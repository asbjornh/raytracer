module rec Pattern

open System
open System.Numerics

open Matrix
open Tuple
open Util
open Transform

type PatternType =
  | Stripes
  | Checkers
  | Ring
  
let patternPoint objectT patternT point =
  transform (inverse objectT) point
  |> transform (inverse patternT)

let patternAt a b (pattern: PatternType) (point: Vector4) =
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


let gradientAt a b sharpness (point: Vector4) =
  let amount = point.X - MathF.Floor point.X |> float
  if (sharpness = 0.) then
    Color.mix amount a b
  else
    let p1 = 0. - sharpness
    let p2 = 1. + sharpness
    let mapped = cubicBezier p1 p2 amount |> clamp 0. 1.
    Color.mix mapped a b
