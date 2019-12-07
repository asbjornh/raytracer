module Clock

open System
open Tuple
open Util
open Transform
open Matrix

let run () =
  let points =
    [0..11]
    |> List.map (fun i -> 
      let degrees = 360. / 12. * (float i)
      let p = point 0. 0. 0.
      let transform = chain [
        translateY 30.
        rotateZ (rad degrees)
        translate 100. 50. 0.
      ]
      multiplyT transform p
      |> Tuple.Map (Math.Round >> int)
      |> to2d
    )

  let white = Color.color 1.0 1.0 1.0

  let canvas = Canvas.canvas 200 100

  points
  |> List.fold (fun canv (x, y) -> Canvas.write x y white canv) canvas
  |> Canvas.toPpm
  |> writeFile @"./out.ppm"