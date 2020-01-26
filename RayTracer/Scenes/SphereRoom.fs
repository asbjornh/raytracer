module SphereRoom

open Camera
open Color
open Light
open Material
open Materials
open Shape
open Transform
open Tuple
open Util
open World

let room =
  cube
  <| chain [translateY 20.f; uniformScale 20.f]
  <| material white 0.2 0.9 0.

let plasticBall =
  sphere
  <| translate -1.5f 1.f 4.f
  <| Fresnel {
    a = material (orange) 0. 1. 0.2
    b = Reflective
    blend = Screen
    power = 1.5
    mixOuter = 1.
    mixInner = 1.
  }

let metalBall =
  sphere
  <| translate 1.5f 1.f 4.f
  <| Blend {
    mode = Multiply
    a = gold
    b = Luminance (mix Color.gold white 0.8)
  }

let glassMat = Fresnel {
  a = coloredGlass orange
  b = Reflective
  blend = Screen
  power = 2.
  mixOuter = 1.
  mixInner = 1.
}

let glassBall = 
  sphere
  <| translateY 1.f
  <| glassMat

let lightPos = point 0. 20. -6.
let light = softLight lightPos ((point 0. 0. 0.) - lightPos) (mix yellow white 0.97) 4 30.f
let w =
  ambientWorld
  <| Some (mix (gray 0.1) blue 0.3, Lighten)
  <| [light]
  <| [glassBall; plasticBall; metalBall; room]

let cam =
  camera 400 225 (rad32 20.f)
  <| (point 0. 2. -19.) <| (point 0. 1. 0.)
let options =
  { defaultOptions with 
      antiAliasing = false }

let run () =
  render options cam w
