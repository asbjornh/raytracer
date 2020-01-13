module ToyCar

open System

open Camera
open Color
open Light
open ObjParser
open Material
open Shape
open Transform
open Tuple
open Util
open World


let lightPos = point 100. 70. -60.
let pLight = pointLight lightPos white
let pLight2 = pointLight (point -20. 5. -20.) (Color.scale 0.2 cyan)
let cLight = constantLight blue Lighten
let cam =
  camera 400 300 (rad32 30.f)
  <| (point -15. 10. -20.) <| (point 0. 1. 0.)

let car =
  importObj "../models/car.obj"
  <| rotateY (rad32 -90.f)
  <| material white 0.2 0.8 0.

let floor =
  plane
  <| chain [ translateY -0.5f; uniformScale 10.f ]
  <| InvisFloor { shadowColor = blue }

let w = 
  { world [pLight; pLight2; cLight] [car; floor]
    with
      background = blue 
      shadows = false }

let aoOptions = {
  samples = 24
  color = mix black blue 0.6
  opacity = 1.
  threshold = 4.f
}

let options =
  { defaultOptions with 
      ambientOcclusion = Some aoOptions 
      antiAliasing = true }

let run () =
  render options cam w
