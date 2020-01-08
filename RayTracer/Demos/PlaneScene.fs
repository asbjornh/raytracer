module PlaneScene

open System

open Camera
open Color
open Light
open Material
open Matrix
open Shape
open Transform
open Tuple
open World


let mat = materialC (color 0.5 0.1 0.6)

let floor = plane (scale 10000.f 1.f 10000.f) mat

let middle =
  sphere
  <| (translate -0.5f 1.f 0.5f)
  <| defaultMaterial ()

let right =
  sphere
  <| (chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ])
  <| defaultMaterial ()

let left =
  sphere
  <| (chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ])
  <| defaultMaterial ()

let darkBlue = color 0. 0.1 0.2
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBlue Add
let cam =
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor]

let w = {
  world [pLight; cLight] objects with
    background = darkBlue
}

let run () =
  render defaultOptions cam w
