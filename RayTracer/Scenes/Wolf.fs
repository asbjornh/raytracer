module Wolf

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

let wolf =
  importObj "../models/wolf.obj"
  <| identity
  <| material white 0.07 1. 0.

let backdropMat =
  gradient
  <| Luminance (Color.scale 2. red)
  <| Luminance (Color.scale 1.5 (mix 0.1 blue cyan))
  <| chain [uniformScale 2.f; translateX 0.5f]
let backdrop =
  plane
  <| chain [
      translate 450.f 470.f 1000.f; 
      rotateX (rad32 -90.f)
      rotateY (rad32 45.f)
      uniformScale 330.f
    ]
  <| backdropMat

let lightPos = point 1000. 600. -200.
let sLight = pointLight lightPos (mix 0.85 yellow white)
let cam =
  camera 400 240 (rad32 30.f)
  <| (point 450. 470. -1100.) <| (point 450. 470. 0.)
let w = {
  world [sLight] [wolf; backdrop] with
    ambientLight = Some (Color.scale 0.1 blue, Add)
}

let aoOptions = {
  samples = 8
  opacity = 1.
  color = black
  threshold = 500.f
}
let options =
  { defaultOptions with 
      antiAliasing = true 
      ambientOcclusion = Some aoOptions }

let run () =
  render options cam w
