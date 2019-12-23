module Textures

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


let marble t = Blend {
  a = materialC white
  b = texture "../tex/checkers.png" (1., 1.) (0., 0.) t
  mode = Multiply
}

let mat =
  marble
  <| chain [translateX 5.; scale 1. 0.5 0.5]

let floor = { defaultPlane () with material = mat }

let middle =
  sphere
  <| (translation 0. 1. 0.)
  <| marble (scaling 1. 1. 1.)

let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| defaultMaterial ()

let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| defaultMaterial ()

let darkBlue = color 0. 0.1 0.2
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBlue true
let cam = camera 100 100 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 2. -3.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects =
  [floor; middle]

let w = {
  world [pLight; cLight] objects with
    background = darkBlue
}

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
