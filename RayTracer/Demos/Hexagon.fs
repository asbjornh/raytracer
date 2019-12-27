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
  sphereT <| chain [translate -0.5 0. -0.9; uniformScale 0.25]

let edge =
  cylinder
  <| chain [
    translateZ -0.9
    rotateZ (-Math.PI / 2.)
    rotateY (-Math.PI / 6.)
    scale 0.25 0.5 0.25
  ]
  <| defaultMaterial ()

let side t =
  group [corner; edge] t <| defaultMaterial ()

let hexagon =
  List.init 6 (fun i ->
    side <| rotateY (float i * Math.PI / 3.)
  )
  |> group <| identity () <| defaultMaterial ()

let pLight = pointLight (point -10. 10. -10.) white
let cam = camera 100 100 (Math.PI / 3.)
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
