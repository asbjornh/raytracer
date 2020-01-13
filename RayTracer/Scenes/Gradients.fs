module Gradients

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

let mat = material blue 0.3 0.8 0.8

let floor = plane <| uniformScale 1000.f <| mat

let wall =
  plane
  <| chain [translateZ 10.f; uniformScale 100.f; rotateX (rad32 80.f)]
  <| mat



let purple = color 0. 0. 0.2
let midMat =
  gradient (materialC pink) (materialC yellow) 
    (chain [
      rotateZ (rad32 90.f)
      uniformScale 2.f
      translateX 0.5f
    ])

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| midMat

let rightMat =
  gradient (materialC white) (materialC pink)
    (chain [
      rotateZ (rad32 90.f)
      uniformScale 2.f
      translateX 0.5f
    ])

let right =
  sphere
  <| (chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ])
  <| rightMat
let leftMat =
  gradient (materialC white) (materialC cyan)
    (chain [
      rotateZ (rad32 90.f)
      uniformScale 2.f
      translateX 0.5f
    ])

let left =
  sphere
  <| (chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ])
  <| leftMat

let constantColor = Color.scale 0.5 (color 0.4 0.4 1.)
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight constantColor Add
let cam =
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let objects = [middle; right; left; floor; wall]

let w = world [pLight; cLight] objects

let run () =
  render defaultOptions cam w
