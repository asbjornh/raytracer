module Luma

open Camera
open Color
open Light
open Material
open Materials
open Matrix
open ObjParser
open Shape
open Transform
open Tuple
open Util
open World

let backDrop =
  plane
  <| chain [
    translate 0.f 2.f 5.f
    rotateX (rad32 90.f)
    uniformScale 10.f
  ]
  <| luminanceTex "../tex/stars.jpg" (0.2, 0.15, 0.5, 0.5)

let body =
  importObj "../models/luma/body.obj"
  <| identity
  <| nintendo
    (Color.scale 0.6 pink)
    (Color.scale 0.6 yellow)
    (mix 0.5 white yellow |> Color.scale 0.3)
    (mix 0.4 white cyan |> Color.scale 2.)

let eyeMat =
  materialRaw
    (gray 0.1) (mix 0.3 black blue) white 10.

let leftEye =
  importObj "../models/luma/eye-left.obj"
  <| identity
  <| eyeMat

let rightEye =
  importObj "../models/luma/eye-right.obj"
    <| identity
    <| eyeMat

let luma =
  groupT [body; leftEye; rightEye]
  <| chain [
    translateX 0.4f
    translateY 0.3f
    rotateX (rad32 -5.f)
    rotateZ (rad32 10.f)
    rotateY (rad32 180.f)
    uniformScale 0.5f
  ]

let light = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 600 400 (rad32 35.f)
  <| (point 0. 2. -12.) <| (point 0. 2. 0.)
  
let w = 
  { world [light] [backDrop; luma] with
      shadows = false
      background = black
  }

let options =
  { defaultOptions with 
      antiAliasing = false 
  }

let FX image =
  let blurred = Blur.boxBlur 60 image
  let depth =
    renderDepth options 11. 14. cam w |> Canvas.map invert
  let glow = Canvas.blendLayers Multiply blurred depth
  Canvas.blendLayers Lighten image glow

let run () =
  renderFX options cam w FX
