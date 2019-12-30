module Teapot

open System

open Camera
open Color
open Light
open ObjParser
open Matrix
open Material
open Shape
open Transform
open Tuple
open Util
open World


let darkBrown = Color.scale 0.25 (color 1. 0.3 0.4)
let lightPos = point -10. 20. -10.
let origin = point 0. 0. 0.
let sLight = softLight lightPos (origin - lightPos) white 5 8.
let cam = camera 50 35 (Math.PI / 3.)
let cTransform = viewTransform (point 0. 25. -30.) (point 2.5 5. 0.) (vector 0. 1. 0.)
cam.transform <- cTransform

let teapot =
  objFromFile "../models/teapot-grouped.obj"
  <| chain [rotateY (rad 35.)]

let objects =
  [teapot]

let w =  world [sLight] objects


let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
