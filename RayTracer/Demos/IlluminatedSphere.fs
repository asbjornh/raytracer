module IlluminatedSphere

open System
open ShellProgressBar

open Canvas
open Color
open Intersection
open Light
open Material
open Ray
open Sphere
open Transform
open Tuple
open Util

let run () =
  let bg = color 0.2 0.2 0.4
  let t = chain [translate 100. 100. 0.; uniformScale 80.]
  let mat = {
    material () with
      color = (color 1. 0.4 1.)
      ambient = 0.
  }
  let s = { sphere () with transform = t; material = mat }

  let l = pointLight (point -100. -100. 250.) (color 1. 1. 1.)

  let c = canvas 200 200
  let len = Canvas.length c
  let w = Canvas.width c

  let progressMsg row col =
    sprintf "Rendering %i of %i pixels" (w * row + col + 1) len

  let bar = new ProgressBar (Canvas.length c, "Rendering", ConsoleColor.Yellow)

  c
  |> Canvas.mapi (fun x y ->
    bar.Tick (progressMsg y x)
    let origin = (point (float x) (float y) 100.)
    let direction = vector 0. 0. -1.
    let r = ray origin (normalize direction)
    let h = intersect s r |> Intersection.hit

    match h with
    | Some i ->
      let point = position i.t r
      let normalV = normal point i.object
      let eyeV = negate r.direction
      lighting l point eyeV normalV s.material
      |> add bg
    | None -> black |> add bg |> Color.scale 0.8
  )
  |> Canvas.toPpm
  |> writeFile @"./out.ppm"

  printfn "\n"