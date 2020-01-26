module Mario

open Camera
open Color
open Light
open Material
open Materials
open ObjParser
open Transform
open Tuple
open Util
open World

let t = chain [uniformScale 0.05f; rotateY (rad32 160.f)]
let pigSkin = color 1. 0.7 0.58
let face =
  importObj "../models/mario/face.obj" t
  <| nintendo
      (color 0.5 0.26 0.27)
      (Color.scale 0.6 pigSkin)
      (Color.scale 0.2 pigSkin)
      (add pigSkin white |> Color.scale 0.8)

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

let hat =
  importObj "../models/mario/hat.obj" t
  <| nintendo
      (color 0.3 0. 0.0)
      (color 0.6 0.1 0.2)
      black
      (Color.scale 2. white)

let mario =
  [ face
    mustache
    leftEye
    rightEye
    leftEyebrow
    rightEyebrow
    hair
    hat
  ]

let sLight = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 200 200 (rad32 60.f)
  <| (point 0. 1.5 -5.) <| (point 0. 2. 0.)
  
let w = 
  { world [sLight] mario with shadows = true }

let options =
  { defaultOptions with 
      antiAliasing = false }

let run () =
  render options cam w
