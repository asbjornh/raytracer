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
open Util
open World


let mat = material yellow 0.3 0.8 0.8

let floor = { defaultPlane () with Material = mat }

let wall = plane (chain [translateZ 10.; rotateX (rad 80.)]) mat



let purple = color 0. 0. 0.2
let midPattern = stripePatternT purple white (chain [
  translateX -0.06
  rotateY (Math.PI / 1.5)
  uniformScale 0.05
])
let middle =
  sphere
  <| (translation -0.5 1. 0.5)
  <| patternMaterial midPattern 0.25 0.85 0.85

let rightPattern = stripePatternT purple white (chain [
  rotateZ (Math.PI / 2.)
  uniformScale 0.3
])
let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| patternMaterial rightPattern 0.25 0.85 0.85

let leftPattern = stripePatternT purple white (identity ())
let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| patternMaterial leftPattern 0.25 0.85 0.85

let darkBrown = Color.scale 0.15 (color 1. 0.3 0.6)
let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight darkBrown
let cam = camera 400 200 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects: IShape list =
  [middle; right; left; floor; wall]

let w = {
  world [pLight; cLight] objects with
    background = darkBrown
}

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile @"./out.ppm"
