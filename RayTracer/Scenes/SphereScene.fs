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

let floor = plane (uniformScale 10.f) wallMaterial

let leftWall =
  plane
  <| chain [
    translateZ 5.f
    rotateY (-MathF.PI / 5.3f);
    rotateX (MathF.PI / 2.f)
    uniformScale 10.f
  ]
  <| wallMaterial

let rightWall =
  plane
  <| chain [
    translateZ 6.f
    rotateY (MathF.PI / 3.f)
    rotateX (MathF.PI / 2.f)
    uniformScale 10.f
  ]
  <| wallMaterial

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| defaultMaterial

let right =
  sphere
  <| (chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ])
  <| defaultMaterial

let left =
  sphere
  <| (chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ])
  <| defaultMaterial

let pLight = pointLight (point -10. 10. -10.) (color 1. 0.9 0.7)
let cam = 
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let w =
  ambientWorld
  <| Some (color 0. 0.1 0.2, Add)
  <| [pLight]
  <| [middle; right; left; floor; leftWall; rightWall;]

let run () =
  render defaultOptions cam w
