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



let purple = color 0. 0. 0.2
let midMat = Pattern {
  a = materialC purple
  b = materialC white
  pattern = Stripes
  transform = chain [
    translateX -0.06
    rotateY (Math.PI / 1.5)
    uniformScale 0.05
  ]
}
let middle =
  sphere
  <| (translation -0.5 1. 0.5)
  <| midMat

let rightMat = Pattern {
  a = materialC purple
  b = materialC white
  pattern = Stripes
  transform = chain [
    rotateZ (Math.PI / 2.)
    uniformScale 0.3
  ]
}
let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| rightMat

let leftMat = Pattern {
  a = materialC purple
  b = materialC white
  pattern = Stripes
  transform = identity ()
}
let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| leftMat

let darkBrown = Color.scale 0.15 (color 1. 0.3 0.6)
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBrown true
let cam = camera 200 100 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects: IShape list =
  [middle; right; left; floor; wall]

let w = {
  world [pLight; cLight] objects with
    background = darkBrown
}

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
