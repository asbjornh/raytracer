module SimpleCircle

open System
open ShellProgressBar

open Canvas
open Color
open Intersection
open Material
open Ray
open Shape
open Transform
open Tuple
open Util

let colorFromIntersect (i: Intersection) =
  let c = i.t |> float |> rangeMap (10., 100.) (0., 1.)
  color c c c

let run () =
  let t = chain [translate 100.f 100.f 0.f; uniformScale 80.f]
  let s = sphereT t
  let c = canvas 200 200

  let len = Canvas.length c
  let w = Canvas.width c

  let progressMsg row col =
    sprintf "Rendering %i of %i pixels" (w * row + col + 1) len

  let bar = new ProgressBar (Canvas.length c, "Rendering", ConsoleColor.Yellow)

  c
  |> Canvas.mapi (fun x y _ ->
    bar.Tick (progressMsg y x)
    let origin = (point (float x) (float y) -5.)
    let r = ray origin (vector 0. 0. 1.)
    let h = intersect r s |> Intersection.hit

    match h with
    | Some i -> colorFromIntersect i
    | None -> black
  )
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")

  printfn "\n"