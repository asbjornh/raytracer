module Yoshi

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
    translate 0.f 1.f 5.f
    rotateX (rad32 90.f)
    uniformScale 10.f
  ]
  <| luminanceTex "../tex/stars.jpg" (0.2, 0.15, 0.5, 0.5)

// TODO: Figure out why Y scale is negative
let tex =
  NormalMap {
    mat = Textured {
      textureRaw "../models/yoshi/body-albedo.png" (1., -1., 0., 0.) with
        ambient = 0.6
        diffuse = 0.4
    }
    tex = Texture.read "../models/yoshi/body-normal.png"
    uvTransform = (1., -1., 0., 0.)
  }

let eyesTex =
  Textured {
    textureRaw "../models/yoshi/eye-albedo.png" (1., -1., 0., 0.) with
      ambient = 0.6
      diffuse = 0.4
  }

let leftEye =
  importObj "../models/yoshi/left-eye.obj" identity eyesTex
let rightEye =
  importObj "../models/yoshi/right-eye.obj" identity eyesTex
let nose =
  importObj "../models/yoshi/nose.obj" identity tex
let jaw =
  importObj "../models/yoshi/jaw.obj" identity tex
let hair =
  importObj "../models/yoshi/hair.obj" identity tex
let head =
  importObj "../models/yoshi/head.obj" identity tex

let yoshi =
  group [
    leftEye
    rightEye
    nose
    jaw
    head
    hair
  ]
  <| rotateY (rad32 120.f)
  <| defaultMaterial

let light = pointLight (point -10. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 300 200 (rad32 35.f)
  <| (point 0. 1. -8.) <| (point 0. 1. 0.)
  
let w = 
  { world [light] [yoshi] with
      shadows = false
      background = black
  }

let options =
  { defaultOptions with 
      antiAliasing = false 
  }

let run () =
  render options cam w
