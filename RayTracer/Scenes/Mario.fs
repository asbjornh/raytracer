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

let backDrop =
  plane
  <| chain [
    translate 0.f 2.f 4.f
    uniformScale 5.5f
    rotateX (rad32 -90.f)
  ]
  <| Gradient {
    a = Luminance blue
    b = Luminance (mix cyan white 0.5)
    sharpness = 0.1
    transform = chain [
      uniformScale 2.f
      translateZ 0.5f
      rotateY (rad32 90.f)
    ]
  }

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

let hairMat =
  nintendo
    (color 0.15 0.03 0.03)
    (color 0.3 0.16 0.1)
    (gray 0.2)
    (Color.scale 3. white)
let mustache =
  importObj "../models/mario/mustache.obj" t hairMat
let hair =
  importObj "../models/mario/hair.obj" t hairMat

let eyeMat =
  nintendo
    (mix blue (gray 0.5) 0.5)
    white black
    (Color.scale 2. white)
let leftEye =
  importObj "../models/mario/eye-left.obj" t eyeMat

let rightEye =
  importObj "../models/mario/eye-right.obj" t eyeMat

let leftEyebrow =
  importObj "../models/mario/eyebrow-left.obj" t hairMat

let rightEyebrow =
  importObj "../models/mario/eyebrow-right.obj" t hairMat

let badge =
  importObj "../models/mario/hat-badge.obj" t eyeMat

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
    leftEye
    rightEye
    leftEyebrow
    rightEyebrow
    hair
    hat
    badge
  ]

let sLight = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 200 200 (rad32 60.f)
  <| (point 0. 2. -4.8) <| (point 0. 2. 0.)
  
let w = 
  { world [sLight] ([backDrop] @ mario) with
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
