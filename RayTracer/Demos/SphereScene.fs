module SphereScene

open System

open Camera
open Color
open Light
open Material
open Shape
open Transform
open Tuple
open World


let wallMaterial = materialC (color 0.5 0.1 0.6)

let floor = sphere (scaling 10. 0.01 10.) wallMaterial

let leftWall =
  sphere
  <| chain [
    translate 0. 0. 5.;
    rotateY (-Math.PI / 4.);
    rotateX (Math.PI / 2.)
    scale 10. 0.01 10.
  ]
  <| wallMaterial

let rightWall =
  sphere
  <| chain [
    translate 0. 0. 5.
    rotateY (Math.PI / 4.)
    rotateX (Math.PI / 2.)
    scale 10. 0.01 10.
  ]
  <| wallMaterial

let middle =
  sphere
  <| (translation -0.5 1. 0.5)
  <| defaultMaterial ()

let right =
  sphere
  <| (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])
  <| defaultMaterial ()

let left =
  sphere
  <| (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])
  <| defaultMaterial ()

let pLight = pointLight (point -10. 10. -10.) (color 1.1 1.1 0.9)
let cLight = constantLight (color 0. 0.04 0.08)
let cam = camera 200 100 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects =
  [middle; right; left; floor; leftWall; rightWall;]

let w = world [pLight; cLight] objects

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile @"./out.ppm"
