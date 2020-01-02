module Hexagon

open System

open Camera
open Color
open Light
open Material
open Matrix
open Shape
open Transform
open Tuple
open Util
open World

let corner =
  sphereT <| chain [translate -0.5f 0.f -0.9f; uniformScale 0.25f]

let edge =
  cylinder
  <| chain [
    translateZ -0.9f
    rotateZ (-MathF.PI / 2.f)
    rotateY (-MathF.PI / 6.f)
    scale 0.25f 0.5f 0.25f
  ]
  <| defaultMaterial ()

let side t =
  group [corner; edge] t <| defaultMaterial ()

let hexagon =
  List.init 6 (fun i ->
    side <| rotateY (float32 i * MathF.PI / 3.f)
  )
  |> group <| identity () <| defaultMaterial ()

let pLight = pointLight (point -10. 10. -10.) white
let cam = camera 100 100 (MathF.PI / 3.f)
let cTransform = viewTransform (point 1. 2. -2.) (point 0. 0. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let w =  world [pLight] [hexagon]

let getParent (s: Shape) =
  match s.parent with
  | Some p -> p
  | None -> failwith "No parent"
let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
