module AnimatedSphere

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

let frame (bar: ProgressBar) l fileName =
  let size = 100.
  let bg = color 0.2 0.2 0.4
  let t = chain [translate size size 0.; uniformScale (0.8 * size)]
  let mat = {
    material () with
      color = (color 1. 0.4 1.)
      ambient = 0.
  }
  let s = { sphere () with transform = t; material = mat }

  let t2 = chain [translate (1.4 * size) (1.4 * size) (0.4 * size); uniformScale (0.4 * size)]
  let mat2 = {
    material () with
      color = (color 0.9 0.6 0.2)
  }
  let s2 = { sphere() with transform = t2; material = mat2 }

  let cSize = (int size) * 2
  let c = canvas cSize cSize
  let len = Canvas.length c
  let w = Canvas.width c

  let progressMsg row col =
    sprintf "Rendering %i of %i pixels" (w * row + col + 1) len

  let childBar = bar.Spawn (len, "Rendering")

  c
  |> Canvas.render (fun x y ->
    childBar.Tick (progressMsg y x)
    let origin = (point (float x) (float y) 200.)
    let direction = vector 0. 0. -1.
    let r = ray origin (normalize direction)

    let h =
      List.concat [intersect r s; intersect r s2]
      |> Intersection.hit

    match h with
    | Some i ->
      let point = position i.t r
      let normalV = normal point i.object
      let eyeV = negate r.direction
      lighting l point eyeV normalV i.object.material
      |> add bg
    | None -> black |> add bg |> Color.scale 0.8
  )
  |> Canvas.toPpm
  |> writeFile ("./frames/" + fileName)


let run () =
  let lights1 =
    keyframe (point -200. 0. 250.) (point -200. 50. -500.) 20

  let lights2 =
    keyframe (point -200. 50. -500.) (point 100. 100. -500.) 11
    |> List.tail

  let lights =
    List.concat [lights1; lights2]
    |> List.rev
    |> List.map (fun pos -> pointLight pos (color 1. 1. 1.))

  let bar = new ProgressBar (List.length lights, "Rendering", ConsoleColor.Yellow)

  lights |> List.iteri (fun i l ->
    bar.Tick(sprintf "Rendering frame %i of %i" (i + 1) (List.length lights))
    let fileName = (string i).PadLeft (3, '0') + ".ppm"
    frame bar l (string fileName)
  )

  printfn "\n"
