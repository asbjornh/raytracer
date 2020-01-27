module Mario

open Camera
open Color
open Light
open Material
open Materials
open ObjParser
open Shape
open Transform
open Tuple
open Util
open World

let t =
  chain [
    uniformScale 0.05f
    rotateX (rad32 5.f)
    rotateY (rad32 160.f)
  ]
let pigSkin = color 1. 0.7 0.58
let face =
  importObj "../models/mario/face.obj" t
  <| nintendo
      (color 0.5 0.26 0.27)
      (Color.scale 0.6 pigSkin)
      (Color.scale 0.2 pigSkin)
      (add pigSkin white |> Color.scale 0.9)

let hair =
  importObj "../models/mario/hair.obj" t
  <| nintendo
      (color 0.15 0.03 0.03)
      (color 0.3 0.16 0.1)
      (gray 0.2)
      (Color.scale 3. white)

let facialHair =
  nintendo
    (color 0.03 0.01 0.02)
    (color 0.1 0.05 0.02)
    (gray 0.2)
    (Color.scale 3. white)

let mustache =
  importObj "../models/mario/mustache.obj" t facialHair

let eyeBallMat uvTransform =
  Blend {
    mode = Multiply
    a = nintendo
      (mix blue (gray 0.5) 0.5)
      white black
      (Color.scale 2. white)
    b = luminanceTex "../models/mario/eye-color.png" uvTransform
  }
let leftEye =
  sphere
  <| chain [
    translate -0.045f 1.7f -0.8f
    rotateX (rad32 -1.f)
    rotateY (rad32 -6.f)
    scale 0.3f 0.7f 0.1f
  ]
  <| eyeBallMat (0.55, 0.45, -0.05, -0.57)

let rightEye =
  sphere
  <| chain [
    translate 0.63f 1.7f -0.56f
    rotateX (rad32 -1.f)
    rotateY (rad32 -30.f)
    scale 0.3f 0.7f 0.1f
  ]
  <| eyeBallMat (0.55, 0.45, 0.1, -0.57)

let leftEyebrow =
  importObj "../models/mario/eyebrow-left.obj" t facialHair

let rightEyebrow =
  importObj "../models/mario/eyebrow-right.obj" t facialHair

let badge =
  importObj "../models/mario/hat-badge.obj" t
  <| nintendo
      (mix blue (gray 0.5) 0.5)
      white black
      (Color.scale 2. white)

let hat =
  importObj "../models/mario/hat.obj" t
  <| nintendo
      (color 0.3 0. 0.0)
      (color 0.6 0.1 0.2)
      black
      (Color.scale 1.7 white)

let mario =
  [ face
    mustache
    leftEyebrow
    rightEyebrow
    hair
    hat
    badge
  ]

let sLight = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 400 400 (rad32 60.f)
  <| (point 0. 2. -4.8) <| (point 0. 2. 0.)
  
let w = 
  { world [sLight] ([leftEye; rightEye] @ mario) with
      shadows = true
      background = gray 0.12
  }

let aoOptions =
  {
    samples = 4
    color = black
    opacity = 0.7
    threshold = 0.1f
  }

let options =
  { defaultOptions with 
      antiAliasing = false 
      ambientOcclusion = None
  }

let run () =
  render options cam w
