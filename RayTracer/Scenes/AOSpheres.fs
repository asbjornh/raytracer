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
let cam = 
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -10.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor; leftWall; rightWall]

let w = 
  { world [pLight] objects with 
      ambientLight = Some (darkBlue, Add)
      shadows = false }

let options = 
  { defaultOptions with
      antiAliasing = false }

let FX image =
  renderOcclusion options 24 8.f cam w
  |> applyOcclusion 0.8 (mix 0.1 darkBlue white) image

let run () =
  renderFX options cam w FX
