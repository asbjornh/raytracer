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


let wallMaterial = material ()
wallMaterial.color <- color 1. 0.9 0.9

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

let middle = sphereT (translation -0.5 1. 0.5)

middle.Material.color <- color 0.1 1. 0.5
middle.Material.diffuse <- 0.7
middle.Material.specular <- 0.3

let right =
  sphereT (chain [ translate 1.5 0.5 -0.5; uniformScale 0.5 ])

right.Material.color <- color 0.5 1. 0.1
right.Material.diffuse <- 0.7
right.Material.specular <- 0.3

let left =
  sphereT (chain [ translate -1.5 0.33 -0.75; uniformScale 0.33 ])

left.Material.color <- color 1. 0.8 0.1
left.Material.diffuse <- 0.7
left.Material.specular <- 0.3

let light = pointLight (point -10. 10. -10.) (color 1. 1. 1.)
let cam = camera 100 50 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let objects =
  [middle; right; left; floor; leftWall; rightWall]
  |> List.map (fun o -> (o :> IShape))

let w = world light objects

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile @"./out.ppm"
