module Textures

open System

open Camera
open Color
open Light
open Material
open Materials
open Matrix
open Shape
open Transform
open Tuple
open World


let i = identity

let uvTransform = (0.4, 0.8, 0., 0.)
let mat = metalGate 0.6 1.5 (0.4, 0.8, 0., 0.)

let ball =
  sphere i mat

let pLight = pointLight (point -10. 7. 2.) (color 1. 0.9 0.7)
let pLight2 = pointLight (point 5. 0. -2.) (color 0.8 0.9 1.)
let cam = 
  camera 500 500 (MathF.PI / 3.f)
  <| (point 0. 0. -3.) <| (point 0. 0. 0.)

let w = {
  world [pLight; pLight2] [ball] with
    background = black
    shadows = false
}

let options =
  { defaultOptions with
      antiAliasing = false }

let run () =
  render options cam w
