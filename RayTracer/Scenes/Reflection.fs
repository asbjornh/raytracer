module Reflection

open System

open Camera
open Color
open Light
open Material
open Shape
open Transform
open Tuple
open World


let wallMaterial = material red 0.1 0.9 0.

let floor = sphere (scale 10.f 0.01f 10.f) wallMaterial

let leftWall =
  sphere
  <| chain [
    translateZ 5.f
    rotateY (-MathF.PI / 5.3f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let rightWall =
  sphere
  <| chain [
    translateZ 6.f
    rotateY (MathF.PI / 3.f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let sphereMat = Fresnel {
  a = materialC white
  b = Reflective Add
  power = 3.
  mixOuter = 1.
  mixInner = 1.
}

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| sphereMat

let right =
  sphere
  <| chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ]
  <| sphereMat

let left =
  sphere
  <| chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ]
  <| sphereMat

let lightPos = (point -10. 10. -10.)
let origin = (point 0. 0. 0.)
let sLight = softLight lightPos origin (color 1. 1. 0.9) 5 8.f
let cLight = constantLight (color 0. 0.15 0.3) Lighten
let lights = [sLight; cLight]

let cam =
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor; leftWall; rightWall;]

let w = world lights objects

let options =
  { defaultOptions with 
      antiAliasing = false }

let run () =
  render options cam w
