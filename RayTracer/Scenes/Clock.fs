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
      let degrees = 360.f / 12.f * (float32 i)
      let p = point 0. 0. 0.
      let t = chain [
        translate 100.f 50.f 0.f
        rotateZ (rad32 degrees)
        translateY 30.f
      ]
      transform t p |> Tuple.toPixel
    )

  let white = Color.color 1.0 1.0 1.0

  let canvas = Canvas.canvas 200 100

  points
  |> List.fold (fun canv (x, y) -> Canvas.write x y white canv) canvas
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")