module SphereRing

open System

open Camera
open Color
open Light
open Material
open Shape
open Transform
open Tuple
open Util
open World


let sphereRing direction t count spread =
  let up = vector 0. 0. 1.
  List.init count (fun i ->
    let degrees = 360. / (float count) * (float i)
    let transform = chain [
      rotateAlign up direction
      rotateZ (rad degrees)
      translateY spread
    ]
    sphereT (Matrix.multiply t transform)
  )


let wallMaterial = materialC (color 0.5 0.1 0.6)

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

let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cLight = constantLight (color 0. 0.1 0.2)
let cam = camera 200 100 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 1.5 -5.) (point 0. 1. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let spheresT = chain [translateY 1.; uniformScale 0.2]
let spheres =
  sphereRing (vector 0. 0. 1.) spheresT 8 3.
let objects =
  List.concat [spheres; [floor; leftWall; rightWall;]]

let w = world [pLight; cLight] objects

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile @"./out.ppm"
