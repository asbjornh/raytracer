module Refraction

open Camera
open Color
open Light
open Material
open Matrix
open Pattern
open Shape
open Transform
open Tuple
open Util
open World

let baseMat = Mix {
  a = material (color 0. 0.15 0.75) 1. 0.0 1.
  b = Transparent { index = 1.17f; blend = Multiply }
  mix = 1.
}
let ballMat = Fresnel {
  a = baseMat
  b = Reflective { blend = Add }
  power = 3.
  mixOuter = 1.
  mixInner = 1.
}

let ball = sphere identity ballMat

let wallMat = material white 1. 0. 0.

let roof =
  plane
  <| chain [ translateY 30.f; scale 29.5f 29.5f 1000.f ]
  <| wallMat
let floor =
  plane
  <| chain [ translateY -30.f; scale 29.5f 29.5f 1000.f ]
  <| wallMat
let wallL =
  plane
  <| chain [ translateX -30.f; scale 29.5f 29.5f 1000.f; rotateZ (rad32 -90.f) ]
  <| wallMat
let wallR =
  plane
  <| chain [ translateX 30.f; scale 29.5f 29.5f 1000.f; rotateZ (rad32 -90.f); ]
  <| wallMat
let backdropMat = Blend {
  a = materialC white
  b = texture "../tex/checkers.png" (0.15, 0.15) (0., -0.5) identity
  mode = Multiply
}
  

let backdrop =
  plane
  <| chain [ translateZ 80.f; uniformScale 42.f; rotateX (rad32 90.f) ]
  // <| chain [ translateY 80.f; uniformScale 100.f; rotateX (rad32 90.f) ]
  <| backdropMat

let light = pointLight (point -10. 10. -10.) white
let objects = [backdrop; ball; floor; roof; wallL; wallR]
let w = world [light] objects

let cam =
  camera 200 200 (rad32 30.f)
  <| (point 0. 0. -5.) <| (point 0. 0. 0.)

let run () =
  renderProgress cam w
  |> Canvas.toPpm
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
