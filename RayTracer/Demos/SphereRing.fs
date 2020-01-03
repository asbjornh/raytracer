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
  let up = vector32 0.f 0.f 1.f
  List.init count (fun i ->
    let degrees = 360.f / (float32 count) * (float32 i)
    let transform = chain [
      rotateAlign up direction
      rotateZ (rad32 degrees)
      translateY spread
    ]
    sphereT (Matrix.multiply t transform)
  )


let wallMaterial = materialC (color 0.5 0.1 0.6)

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

let pLight = pointLight (point32 -10.f 10.f -10.f) (color 1. 0.9 0.7)
let cLight = constantLight (color 0. 0.1 0.2) true
let cam = camera 400 200 (MathF.PI / 3.f)
let cTransform = viewTransform (point32 0.f 1.5f -5.f) (point32 0.f 1.f 0.f) (vector32 0.f 1.f 0.f)
cam.transform <- cTransform

let spheresT = chain [translateY 1.f; uniformScale 0.2f]
let spheres =
  sphereRing (vector32 0.f 0.f 1.f) spheresT 8 3.f
let objects =
  List.concat [spheres; [floor; leftWall; rightWall;]]

let w =  world [pLight; cLight] objects 

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
