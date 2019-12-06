module Projectile

open System
open Tuple
open Util

type Projectile = Tuple * Tuple
type Environment = Tuple * Tuple

let tick (e: Environment) (p: Projectile) =
  let (gravity, wind) = e
  let (position, velocity) = p
  let newPos = add position velocity
  let newVel = velocity |> add gravity |> add wind
  Projectile (newPos, newVel)

let toInt (a: float) = Math.Round a |> int
let to2d (x, y, _, _) = (x, y)
let toCanvasPoint = (Tuple.map toInt) >> to2d

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
  |> List.map toCanvasPoint
  |> List.fold (fun canv (x, y) -> Canvas.write x y white canv) canvas
  |> Canvas.toPpm
  |> writeFile @"./out.ppm"