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
  <| chain [translateY 20.f; translateZ -6.f; uniformScale 20.f]
  <| material white 0.2 0.9 0.

let plastic = Blend {
  mode = Add
  a = specularOnly 0.5 50.
  b = Fresnel {
    a = glitterHighlight orange 0. 1. 0.7 (0.75, 1.5, 0., 0.)
    b = Reflective None
    blend = Screen
    power = 1.5
    mixOuter = 1.
    mixInner = 1.
  }
}
let plasticBall =
  sphere
  <| translate -1.5f 1.f 4.f
  <| plastic

let metalBall =
  sphere
  <| translate 1.5f 1.f 4.f
  <| Blend {
      mode = Multiply
      a = gold 0.3 10. (Some { samples = 4; angle = 0.4 })
      b = Luminance (mix 0.8 Color.gold white)
    }

let glassMat = Fresnel {
  a = coloredGlass orange
  b = Reflective None
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
let light = softLight lightPos ((point 0. 0. 0.) - lightPos) (mix 0.97 yellow white) 10 30.f
let w =
  ambientWorld
  <| Some (mix 0.3 (gray 0.1) blue, Lighten)
  <| [light]
  <| [plasticBall; glassBall; metalBall; room]

let cam =
  camera 500 225 (rad32 25.f)
  <| (point 0. 2. -19.) <| (point 0. 1. 0.)
let options =
  { defaultOptions with 
      antiAliasing = true }

let run () =
  render options cam w
