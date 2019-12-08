module SimpleCircle

open System
open ShellProgressBar

open Canvas
open Color
open Ray
open Sphere
open Transform
open Tuple
open Util

let run () =
  let t = chain [translate 50. 50. 0.; uniformScale 40.]
  let s = sphere () |> transform t
  let c = canvas 100 100

  let len = Canvas.length c
  let w = Canvas.width c

  let progressMsg row col =
    sprintf "Rendering %i of %i pixels" (w * row + col + 1) len

  let bar = new ProgressBar (Canvas.length c, "Rendering", ConsoleColor.Yellow)

  c
  |> Canvas.mapi (fun rowI colI ->
    bar.Tick (progressMsg rowI colI)
    let origin = (point (float rowI) (float colI) -5.)
    let r = ray origin (vector 0. 0. 1.)
    let i = intersect s r

    if (List.isEmpty i)
    then black
    else white
  )
  |> Canvas.toPpm
  |> writeFile @"./out.ppm"

  printfn "\n"