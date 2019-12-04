module Main

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
  printfn "Position: %A, Velocity: %A" newPos newVel
  Projectile (newPos, newVel)

[<EntryPoint>]
let main argv =
  let projectile = (point 0.0 0.0 0.0), (vector 50.0 50.0 0.0)
  let env = (vector 0.0 -9.8 0.0), (vector -1.0 0.0 0.0)

  let imageFile = toPpm (canvas 200 100)
  writeFile @"./out.ppm" imageFile

  let ticks = [1..20] |> List.map (fun _ -> tick env) |> List.reduce (>>)
  ticks projectile |> ignore
  0