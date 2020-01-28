module Mario

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
  <| Pattern {
    a = Luminance (yellow |> mix 0.7 orange)
    b = Luminance (yellow |> mix 0.7 orange |> mix 0.8 white)
    pattern = Pattern.Stripes
    transform = chain [rotateY (rad32 -45.f); uniformScale 0.05f]
  }

let pigSkin = color 1. 0.7 0.58
let face =
  importObj "../models/mario/face.obj" identity
  <| nintendo
      (color 0.5 0.26 0.27 |> Color.scale 1.2)
      (Color.scale 0.5 pigSkin)
      (Color.scale 0.2 pigSkin)
      (add pigSkin white |> Color.scale 1.1)

let hair =
  importObj "../models/mario/hair.obj" identity
  <| nintendo
      (color 0.15 0.03 0.03)
      (color 0.3 0.06 0.06)
      (gray 0.2)
      (Color.scale 2.5 white)

let facialHair =
  nintendo
    (color 0.03 0.01 0.02)
    (color 0.1 0.05 0.02)
    (gray 0.2)
    (Color.scale 3. white)

let mustache =
  importObj "../models/mario/mustache.obj" identity facialHair

let leftEyebrow =
  importObj "../models/mario/eyebrow-left.obj" identity facialHair

let rightEyebrow =
  importObj "../models/mario/eyebrow-right.obj" identity facialHair

let badge =
  importObj "../models/mario/hat-badge.obj" identity
  <| nintendo
      (mix 0.5 blue (gray 0.5))
      white black
      (Color.scale 2. white)

let hat =
  importObj "../models/mario/hat.obj" identity
  <| nintendo
      (color 0.3 0. 0.0)
      (color 0.6 0.1 0.2)
      black
      (mix 0.5 white cyan |> Color.scale 3.)

let symbol =
  importObj "../models/mario/m-symbol.obj" identity
    <| material red 0.5 0.7 0.

let faceGroup =
  groupT
    [
      face
      mustache
      leftEyebrow
      rightEyebrow
      hair
      hat
      badge
      symbol
    ]
    <| chain [
      uniformScale 0.05f
      rotateX (rad32 5.f)
      rotateY (rad32 160.f)
    ]

let eyeMat uOffset vOffset =
  Blend {
    mode = Multiply
    a = nintendo
      (mix 0.5 blue (gray 0.5) |> Color.scale 1.2)
      (Color.scale 0.8 white)
      black
      (Color.scale 2. white)
    b = luminanceTex
      "../models/mario/eye-color.png"
      (0.55, 0.45, uOffset, vOffset)
  }

let rightEye =
  sphere
  <| chain [
    translate -0.045f 1.7f -0.8f
    rotateX (rad32 -1.f)
    rotateY (rad32 -6.f)
    scale 0.3f 0.7f 0.1f
  ]
  <| eyeMat 0.12 -0.57

let leftEye =
  sphere
  <| chain [
    translate 0.63f 1.7f -0.56f
    rotateX (rad32 -1.f)
    rotateY (rad32 -30.f)
    scale 0.3f 0.7f 0.1f
  ]
  <| eyeMat 0.15 -0.57

let mario =
  groupT [faceGroup; rightEye; leftEye]
  <| chain [rotateY (rad32 -5.f); rotateX (rad32 3.f)]

let light = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
// let light = softLight (point -10. 10. -10.) (point 0. 2. 0.) (color 1. 1. 0.9) 2 4.f
let cam =
  camera 300 200 (rad32 35.f)
  <| (point 0. 2. -11.5) <| (point 0. 2. 0.)
  
let w = 
  { world [light] [backDrop; mario] with
      shadows = true
      background = black
  }

let aoOptions =
  {
    samples = 8
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
