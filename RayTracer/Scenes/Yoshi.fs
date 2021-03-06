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
  <| luminanceTex "../models/yoshi/background.jpg" (0.07, 0.05, 0.45, 0.5)

let tex ambient diffuse =
  nintendoTex "../models/yoshi/body-albedo.png" ambient diffuse 2. (Color.scale 2.2 white)

let eyesTex uOffset vOffset =
  Fresnel {
    a = Blend {
      mode = Multiply
      a = Textured {
        textureRaw "../models/yoshi/eye-background.png" (1., -1., 0., 0.) with
          ambient = 0.7
          diffuse = 0.5
      }
      b = luminanceTex "../models/yoshi/eye-pupil.png" (1., -1., uOffset, vOffset)
    }
    blend = Add
    b = Luminance (gray 0.6)
    power = 3.
    mixInner = 1.
    mixOuter = 1.
  }

let leftEye =
  importObj "../models/yoshi/left-eye.obj" identity (eyesTex -0.25 -0.08)
let rightEye =
  importObj "../models/yoshi/right-eye.obj" identity (eyesTex 0.15 -0.02)
let nose =
  importObj "../models/yoshi/nose.obj" identity (tex 0.9 0.5)
let jaw =
  importObj "../models/yoshi/jaw.obj" identity (tex 1. 0.3)
let hair =
  importObj "../models/yoshi/hair.obj" identity (tex 0.9 0.5)
let head =
  importObj "../models/yoshi/head.obj" identity (tex 0.9 0.5)
let mouth =
  importObj "../models/yoshi/mouth.obj" identity
  <| Textured {
      textureRaw "../models/yoshi/body-albedo.png" (1., -1., 0., 0.) with
        ambient = 1.
        diffuse = 1.
    }

let yoshi =
  group [
    leftEye
    rightEye
    nose
    jaw
    head
    hair
    mouth
  ]
  <| chain [
    translateY 0.05f
    rotateX (rad32 5.f)
    rotateY (rad32 120.f)
  ]
  <| defaultMaterial

let light = pointLight (point -7. 10. -10.) (color 1. 1. 0.9)
let cam =
  camera 300 200 (rad32 20.f)
  <| (point 0. 1. -14.) <| (point 0.25 0.9 0.)
  
let w = 
  { world [light] [backDrop; yoshi] with
      ambientLight = Some ((Color.scale 0.2 blue), Lighten)
      shadows = false
      background = black
  }

let options =
  { defaultOptions with 
      antiAliasing = false
  }

let run () =
  render options cam w
