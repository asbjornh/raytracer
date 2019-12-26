module Refraction

open Camera
open Color
open Light
open Material
open Matrix
open Pattern
open Shape
open Transform
open Tuple
open Util
open World

let baseMat = Mix {
  a = material (color 0. 0.15 0.75) 1. 0.0 1.
  b = Transparent { index = 1.17; blend = Multiply }
  mix = 1.
}
let ballMat = Fresnel {
  a = baseMat
  b = Reflective { blend = Add }
  mix = 1.
}

let ball = sphere (identity ()) ballMat

let wallMat = material white 1. 0. 0.

let roof = plane <| translate 0. 30. 0. <| wallMat
let floor = plane <| translate 0. -30. 0. <| wallMat
let wallL = plane <| chain [rotateZ (rad -90.); translateY -30.] <| wallMat
let wallR = plane <| chain [rotateZ (rad -90.); translateY 30.] <| wallMat
let backdropMat = Pattern {
  a = materialC black
  b = materialC white
  pattern = Checkers
  transform = chain [uniformScale 4.]
}
let backdrop = plane <| chain [rotateX (rad 90.); translateY 80.] <| backdropMat

let light = pointLight (point -10. 10. -10.) white
let objects = [backdrop; ball; floor; roof; wallL; wallR]
let w = world [light] objects

let cam = camera 200 200 (rad 30.)
let cTransform = viewTransform (point 0. 0. -5.) (point 0. 0. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
