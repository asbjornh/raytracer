module CaptainToad

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

let uvTransform = (1., -1., 0., 0.)

let bodyMat = Textured {
  textureRaw "../models/captain-toad/body-color.png" uvTransform with
    ambient = 0.5
}

let faceMat = Textured {
  textureRaw "../models/captain-toad/face-color.png" uvTransform with
    ambient = 0.5
}

let hatMat = NormalMap {
  mat = Textured {
    textureRaw "../models/captain-toad/hat-color.png" uvTransform with
      ambient = 0.5
  }
  tex = Texture.read "../models/captain-toad/hat-normal.png"
  uvTransform = uvTransform
}

let lampMat = NormalMap {
  mat = Textured {
    textureRaw "../models/captain-toad/lamp-color.png" uvTransform with
      ambient = 0.5
  }
  tex = Texture.read "../models/captain-toad/lamp-normal.png"
  uvTransform = uvTransform
}

let face =
  importObj "../models/captain-toad/face.obj" identity faceMat
let eyes =
  importObj "../models/captain-toad/eyes.obj" identity defaultMaterial
let mouth =
  importObj "../models/captain-toad/mouth.obj" identity faceMat
let hat =
  importObj "../models/captain-toad/hat.obj" identity hatMat
let scarf =
  importObj "../models/captain-toad/scarf.obj" identity bodyMat
let band =
  importObj "../models/captain-toad/band.obj" identity bodyMat
let lamp =
  importObj "../models/captain-toad/lamp.obj" identity lampMat
let lightBulb =
  importObj "../models/captain-toad/light.obj" identity lampMat

let toad =
  group [
    face
    eyes
    mouth
    hat
    scarf
    band
    lamp
    lightBulb
  ]
  <| chain [
    rotateY (rad32 210.f)
    uniformScale 0.02f
  ]
  <| defaultMaterial

let light = pointLight (point -7. 6. -10.) (color 1. 1. 0.9)
let cam =
  camera 300 200 (rad32 20.f)
  <| (point 0. 1. -14.) <| (point 0. 0.7 0.)
  
let w = 
  { world [light] [toad] with
      shadows = false
      background = black
  }

let options =
  { defaultOptions with 
      antiAliasing = false
  }

let run () =
  render options cam w
