module AOSpheres

open System

open Camera
open Color
open Light
open Material
open Shape
open Transform
open Tuple
open World


let mat = material white 0.5 0.5 0.

let floor = plane (uniformScale 10.f) mat

let leftWall =
  plane
  <| chain [
    translateZ 5.f
    rotateY (-MathF.PI / 5.3f);
    rotateX (MathF.PI / 2.f)
    uniformScale 10.f
  ]
  <| mat

let rightWall =
  plane
  <| chain [
    translateZ 6.f
    rotateY (MathF.PI / 3.f)
    rotateX (MathF.PI / 2.f)
    uniformScale 10.f
  ]
  <| mat

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| mat

let right =
  sphere
  <| (chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ])
  <| mat

let left =
  sphere
  <| (chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ])
  <| mat

let darkBlue = (color 0. 0.1 0.2)
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBlue Add
let cam = 
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -10.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor; leftWall; rightWall]

let w = { world [pLight; cLight] objects with shadows = false }

let aoOptions =
  { color = mix darkBlue white 0.1
    samples = 24
    opacity = 0.8
    threshold = 8.f }
let options = 
  { defaultOptions with
      ambientOcclusion = Some aoOptions }

let run () =
  render options cam w