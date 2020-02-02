module ToyCar

open System

open Camera
open Color
open Light
open ObjParser
open Material
open Shape
open Transform
open Tuple
open Util
open World


let lightPos = point 100. 70. -60.
let pLight = pointLight lightPos white
let pLight2 = pointLight (point -20. 5. -20.) (Color.scale 0.2 cyan)
let cam =
  camera 400 300 (rad32 30.f)
  <| (point -15. 10. -20.) <| (point 0. 1. 0.)

let car =
  importObj "../models/car.obj"
  <| rotateY (rad32 -90.f)
  <| material white 0.2 0.8 0.

let floor =
  plane
  <| chain [ translateY -0.5f; uniformScale 10.f ]
  <| InvisFloor { shadowColor = blue }

let w = {
  world [pLight; pLight2] [car; floor] with
    ambientLight = Some (blue, Lighten)
    background = blue 
    shadows = false
}

let options =
  { defaultOptions with 
      antiAliasing = true }

let FX image =
  renderOcclusion options 24 4.f cam w
  |> applyOcclusion 1. (mix 0.6 black blue) image

let run () =
  renderFX options cam w FX
