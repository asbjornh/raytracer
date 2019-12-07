module Projectile

open System
open Tuple
open Util

type Projectile = Tuple * Tuple
type Environment = Tuple * Tuple

let tick (e: Environment) (p: Projectile) =
  let (gravity, wind) = e
  let (position, velocity) = p
  let newPos = position + velocity
  let newVel = velocity + gravity + wind
  Projectile (newPos, newVel)

let run () =
  let projectile = (point 0.0 100.0 0.0), (vector 2.0 -1.5 0.0)
  let env = (vector 0.0 0.02 0.0), (vector -0.015 0.0 0.0)

  let points =
    [1..150]
    |> List.fold (fun a _ -> (tick env (List.head a)) :: a) [projectile]
    |> List.rev
    |> List.map fst

  let white = Color.color 1.0 1.0 1.0

  let canvas = Canvas.canvas 200 100

  points
  |> List.map Tuple.toPixel
  |> List.fold (fun canv (x, y) -> Canvas.write x y white canv) canvas
  |> Canvas.toPpm
  |> writeFile @"./out.ppm"