module Main

open System
open Tuple
open Canvas
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

[<EntryPoint>]
let main argv =
  let projectile = (point 0.0 100.0 0.0), (vector 15.0 -10.0 0.0)
  let env = (vector 0.0 0.98 0.0), (vector -1.0 0.0 0.0)

  let ticks =
    [1..20]
    |> List.fold (fun a _ -> (tick env (List.head a)) :: a) [projectile]
    |> List.rev
    |> List.map fst

  let pixels =
    ticks
    |> List.filter (fun (x, y, _, _) -> y <= 100.0 && y >= 0.0 && x >= 0.0)
    |> List.map (Tuple.map toInt >> fun (x, y, _, _) -> (x, y))

  printfn "Pixels: %A" pixels

  let white = Color.color 1.0 1.0 1.0
  let c =
    pixels
    |> List.fold (fun acc (x, y) -> Canvas.write x y white acc) (canvas 200 100)

  let imageFile = toPpm c
  writeFile @"./out.ppm" imageFile

  0