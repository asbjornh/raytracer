module Refraction

open Camera
open Color
open Light
open Material
open Materials
open Shape
open Transform
open Tuple
open Util
open World

let room =
  cube
  <| chain [translateY 20.f; uniformScale 20.f]
  <| material white 0. 1. 0.

let ballMat = Fresnel {
  a = coloredGlass (color 0. 0.15 0.75)
  b = Reflective { blend = Add }
  power = 3.
  mixOuter = 1.
  mixInner = 1.
}

let ball = 
  sphere
  <| translateY 1.f
  <| ballMat

let light = pointLight (point -10. 10. -10.) white
let objects = [ball; room]
let w = world [light] objects

let cam =
  camera 200 200 (rad32 30.f)
  <| (point 0. 2. -8.) <| (point 0. 0.5 0.)

let run () =
  render defaultOptions cam w
