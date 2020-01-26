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
let sLight = pointLight lightPos white
let cam =
  camera 400 300 (rad32 30.f)
  <| (point 0. 300. -1300.) <| (point 40. 100. 0.)

let mat = Fresnel {
  a = material pink 0.1 0.9 0.
  b = Luminance yellow
  blend = BlendingMode.Normal
  power = 1.5
  mixOuter = 1.
  mixInner = 1.
}
let deer =
  importObj "../models/Deer.obj"
  <| chain [translateY -100.f; rotateY (rad32 90.f)]
  <| mat

let w = 
  { world [sLight] [deer] with 
      ambientLight = Some (mix blue cyan 0.1, Lighten)
      background = yellow }

let options =
  { defaultOptions with 
      antiAliasing = true }

let run () =
  render options cam w
