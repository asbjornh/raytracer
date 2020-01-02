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
let marble scale = NormalMap {
  mat = Blend {
    a = materialC white
    b = texture "../tex/metal-plate-color.jpg" (scale, scale) (0., 0.) i
    mode = Multiply
  }
  tex = textureRaw "../tex/metal-plate-normal.jpg" (scale, scale) (0., 0.) i
}

let t = rotateAlign (vector 0. 1. 0.) (vector 0. 0. 1.)

let floor = plane <| uniformScale 1000.f <| marble (0.5 / 1000.)

let middle =
  sphere
  <| translateY 1.f
  <| marble 0.5

let right =
  sphere
  <| chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ]
  <| defaultMaterial ()

let left =
  sphere
  <| chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ]
  <| defaultMaterial ()

let darkBlue = color 0.1 0.1 0.2
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight (color 0.25 0.25 0.35) true
let cam = camera 200 200 (MathF.PI / 3.f)
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
