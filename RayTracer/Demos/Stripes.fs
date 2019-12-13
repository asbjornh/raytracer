module Stripes

open System

open Camera
open Color
open Light
open Matrix
open Material
open Pattern
open Shape
open Transform
open Tuple
open World


let mat = materialC (color 0.5 0.1 0.6)

let floor = { defaultPlane () with Material = mat }


let midPattern = stripePatternT darkGray white (chain [
  rotateY (Math.PI / 1.5)
  uniformScale 0.05
])
let middle =
  sphere
  <| (translation -0.5 1. 0.5)
  <| patternMaterial midPattern 0.1 0.9 0.9

let rightPattern = stripePatternT darkGray white (chain [
  rotateZ (Math.PI / 2.)
  uniformScale 0.3
])

let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| patternMaterial rightPattern 0.1 0.9 0.9

let leftPattern = stripePatternT darkGray white (identity ())
let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| patternMaterial leftPattern 0.1 0.9 0.9

let darkBlue = color 0. 0.1 0.2
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBlue
let cam = camera 400 200 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects: IShape list =
  [middle; right; left; floor]

let w = {
  world [pLight; cLight] objects with
    background = darkBlue
}

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile @"./out.ppm"
