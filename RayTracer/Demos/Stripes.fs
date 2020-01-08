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

let floor = plane <| uniformScale 1000.f <| mat

let wall =
  plane
  <| chain [
    translateZ 10.f;
    uniformScale 1000.f;
    rotateX (rad32 80.f)
  ] 
  <| mat



let midMat = Pattern {
  a = Reflective { blend = Normal }
  b = materialC white
  pattern = Stripes
  transform = chain [
    translateX -0.06f
    rotateY (MathF.PI / 1.5f)
    uniformScale 0.1f
  ]
}

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| midMat

let rightMat = Pattern {
  a = materialC white
  b = Reflective { blend = Normal }
  pattern = Stripes
  transform = chain [
    rotateZ (rad32 -15.f)
    rotateY (MathF.PI / -0.5f)
    uniformScale 0.2f
  ]
}
let right =
  sphere
  <| chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ]
  <| rightMat

let leftMat = Pattern {
  a = Reflective { blend = Normal }
  b = materialC white
  pattern = Stripes
  transform = chain [rotateZ (rad32 15.f); uniformScale 0.25f ]
}
let left =
  sphere
  <| chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ]
  <| leftMat

let darkBrown = Color.scale 0.25 (color 1. 0.3 0.4)
let lightPos = point -10. 10. -10.
let origin = point 0. 0. 0.
let sLight = softLight lightPos origin white 5 8.f
let cLight = constantLight darkBrown Add
let cam = 
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor; wall]

let w = {
  world [sLight; cLight] objects with
    background = darkBrown
}

let run () =
  render defaultOptions cam w
