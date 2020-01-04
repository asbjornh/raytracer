module Deer

open System

open Camera
open Color
open Light
open ObjParser
open Matrix
open Material
open Shape
open Transform
open Tuple
open Util
open World


let darkBrown = Color.scale 0.25 (color 1. 0.3 0.4)
let lightPos = point 1000. 400. -600.
let origin = point 0. 0. 0.
let sLight = softLight lightPos (origin - lightPos) white 5 8.f
let cLight = constantLight (mix blue cyan 0.1) false
let cam = camera 400 300 (rad32 30.f)
let cTransform = viewTransform (point 0. 300. -1300.) (point 40. 100. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let mat = Fresnel {
  a = material pink 0.1 0.9 0.
  b = Luminance yellow
  mix = 1.
}
let deer =
  objFromFile "../models/Deer.obj"
  <| chain [translateY -100.f; rotateY (rad32 90.f)]
  <| mat

let w = 
  { world [sLight; cLight] [deer]
    with background = yellow }


let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
