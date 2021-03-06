module Teapot

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
let pLight = pointLight <| point -20. 20. -30. <| white
let cam = 
  camera 400 300 (rad32 8.f)
  <| (point 0. 0. -300.) <| (point 0. 0. 0.)

let mat = Fresnel {
  a = materialRaw
        (mix 0.1 blue red |> Color.scale 0.6)
        (Color.scale 0.8 yellow)
        black 0.
  b = Luminance red
  blend = BlendingMode.Normal
  power = 2.
  mixOuter = 1.
  mixInner = 1.
}
let teapot =
  importObj "../models/teapot-mid.obj"
  <| chain [translateY -7.f]
  <| mat

let w = {
  world [pLight] [teapot] with
    background = red
}


let run () =
  render defaultOptions cam w
