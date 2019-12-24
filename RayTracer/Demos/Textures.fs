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


let i = identity ()
let marble = NormalMap {
  mat = Blend {
    a = materialC white
    b = texture "../tex/metal-plate-color.jpg" (0.5, 0.5) (0., 0.) i
    mode = Multiply
  }
  tex = textureRaw "../tex/metal-plate-normal.jpg" (0.5, 0.5) (0., 0.) i
}

let t = rotateAlignment (vector 0. 1. 0.) (vector 0. 0. 1.)

let floor = { defaultPlane () with material = marble; }

let middle =
  sphere
  <| (translation 0. 1. 0.)
  <| marble

let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| defaultMaterial ()

let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| defaultMaterial ()

let darkBlue = color 0.1 0.1 0.2
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight (color 0.25 0.25 0.35) true
let cam = camera 200 200 (Math.PI / 3.)
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
