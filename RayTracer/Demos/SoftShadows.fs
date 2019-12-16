module SoftShadows

open System

open Camera
open Color
open Light
open Material
open Shape
open Transform
open Tuple
open World

let wallMaterial = material yellow 0.3 0.8 0.8

let floor = sphere (scaling 10. 0.01 10.) wallMaterial

let leftWall =
  sphere
  <| chain [
    translate 0. 0. 5.;
    rotateY (-Math.PI / 5.3);
    rotateX (Math.PI / 2.)
    scale 10. 0.01 10.
  ]
  <| wallMaterial

let rightWall =
  sphere
  <| chain [
    translate 0. 0. 6.
    rotateY (Math.PI / 3.)
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

let lightPos = point -10. 10. -10.
let origin = point 0. 0. 0.
let sLights = squareLight lightPos (origin - lightPos) (color 1. 0.9 0.7) 5 1.5
let darkBrown = Color.scale 0.15 (color 1. 0.3 0.6)
let cLight = constantLight darkBrown
let lights = List.concat [sLights; [cLight]]
let cam = camera 400 200 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects =
  [middle; right; left; floor; leftWall; rightWall;]

let w = world lights objects

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
