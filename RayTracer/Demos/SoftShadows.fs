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

let floor = sphere (scale 10.f 0.01f 10.f) wallMaterial

let leftWall =
  sphere
  <| chain [
    translateZ 5.f
    rotateY (-MathF.PI / 5.3f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let rightWall =
  sphere
  <| chain [
    translateZ 6.f
    rotateY (MathF.PI / 3.f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let middle =
  sphere
  <| translate -0.5f 1.f 0.5f
  <| defaultMaterial ()

let right =
  sphere
  <| chain [ translate 1.5f 0.5f -0.5f; uniformScale 0.5f ]
  <| defaultMaterial ()

let left =
  sphere
  <| chain [ translate -1.5f 0.33f -0.75f; uniformScale 0.33f ]
  <| defaultMaterial ()

let lightPos = point -10. 10. -10.
let origin = point 0. 0. 0.
let sLight = softLight lightPos origin (color 1. 0.9 0.7) 2 3.f
let darkBrown = Color.scale 0.15 (color 1. 0.3 0.6)
let cLight = constantLight darkBrown Add
let lights = [sLight; cLight]
let cam = 
  camera 400 200 (MathF.PI / 3.f)
  <| (point 0. 1.5 -5.) <| (point 0. 1. 0.)

let objects =
  [middle; right; left; floor; leftWall; rightWall;]

let w = world lights objects

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
