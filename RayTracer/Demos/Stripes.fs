module Stripes

open System

open Camera
open Color
open Light
open Matrix
open Material
open Pattern
open Shape
open Transform
open Tuple
open Util
open World


let mat = material yellow 0.3 0.8 0.8

let floor = { defaultPlane () with Material = mat }

let wall = plane (chain [translateZ 10.; rotateX (rad 80.)]) mat



let midMat = Pattern {
  a = Reflective { blend = Normal }
  b = materialC white
  pattern = Stripes
  transform = chain [
    translateX -0.06
    rotateY (Math.PI / 1.5)
    uniformScale 0.1
  ]
}

let middle =
  sphere
  <| (translation -0.5 1. 0.5)
  <| midMat

let rightMat = Pattern {
  a = materialC white
  b = Reflective { blend = Normal }
  pattern = Stripes
  transform = chain [
    rotateZ (rad -15.)
    rotateY (Math.PI / -0.5)
    uniformScale 0.2
  ]
}
let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| rightMat

let leftMat = Pattern {
  a = Reflective { blend = Normal }
  b = materialC white
  pattern = Stripes
  transform = chain [rotateZ (rad 15.); uniformScale 0.25]
}
let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| leftMat

let darkBrown = Color.scale 0.25 (color 1. 0.3 0.4)
let lightPos = point -10. 10. -10.
let origin = point 0. 0. 0.
let sLight = softLight lightPos (origin - lightPos) white 5 8.
let cLight = constantLight darkBrown true
let cam = camera 400 200 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects: IShape list =
  [middle; right; left; floor; wall]

let w = {
  world [sLight; cLight] objects with
    background = darkBrown
}

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
