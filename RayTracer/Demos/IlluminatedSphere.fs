module IlluminatedSphere

open System
open ShellProgressBar

open Canvas
open Color
open Intersection
open Light
open Material
open Ray
open Shape
open Transform
open Tuple
open Util

let run () =
  let bg = color 0.2 0.2 0.4
  let t = chain [translate 100.f 100.f 0.f; uniformScale 80.f]
  let mat = material (color 1. 0.4 1.) 0. 0.9 0.9
  let s = sphere t mat

  let t2 = chain [translate 140.f 140.f 40.f; uniformScale 40.f]
  let mat2 = materialC (color 0.9 0.6 0.2)
  let s2 = sphere t2 mat2

  let l = pointLight (point -100. -100. 250.) (color 1. 1. 1.)

  let c = canvas 200 200
  let len = Canvas.length c
  let w = Canvas.width c

  let progressMsg row col =
    sprintf "Rendering %i of %i pixels" (w * row + col + 1) len

  let bar = new ProgressBar (Canvas.length c, "Rendering", ConsoleColor.Yellow)

  c
  |> Canvas.render (fun x y ->
    bar.Tick (progressMsg y x)
    let origin = (point (float x) (float y) 200.)
    let direction = vector 0. 0. -1.
    let r = ray origin (normalize direction)

    let h =
      List.concat [intersect r s; intersect r s2]
      |> Intersection.hit

    match h with
    | Some i ->
      let point = position i.t r
      let normalV = normalAt i.object point
      let eyeV = negate r.direction
      match i.object.material with
      | Phong mat ->
        lighting l point eyeV normalV mat 0.
      | _ -> black
      |> add bg
    | None -> black |> add bg |> Color.scale 0.8
  )
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")

  printfn "\n"