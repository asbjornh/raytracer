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

let backDrop =
  plane
  <| chain [
    translate 0.f 1.f 5.f
    rotateX (rad32 90.f)
    uniformScale 10.f
  ]
  <| luminanceTex "../models/captain-toad/background.png" (0.15, 0.06, 0.6, 0.35)

let scarfMat =
  nintendoTex "../models/captain-toad/body-color.png" 0.4 0.8 2.
    (mix 0.5 white cyan |> Color.scale 4.5)
let bandMat =
  nintendoTex "../models/captain-toad/body-color.png" 0.7 0.4 2. (Color.scale 2. white)

let mouthMat =
  nintendoTex "../models/captain-toad/face-color.png" 0.7 0.5 2. white

let hatMat =
  nintendoTex "../models/captain-toad/hat-color.png" 0.7 0.5 3.
    (mix 0.5 blue cyan |> mix 0.5 white |> Color.scale 2.5)

let lampMat =
  nintendoTex "../models/captain-toad/lamp-color.png" 0.7 0.4 1.5 (Color.scale 2. white)

let lightMat = Blend {
  mode = Add
  a = luminanceTex "../models/captain-toad/lamp-color.png" uvTransform
  b = Luminance (Color.scale 0.1 white)
}

let eyeMat = Textured {
  textureRaw "../models/captain-toad/eye-color.png" uvTransform with
    ambient = 0.5
}

let pigSkin = color 1. 0.8 0.53
let face =
  importObj "../models/captain-toad/face.obj" identity
  <| nintendo
      (color 0.5 0.26 0.27 |> Color.scale 1.6)
      (Color.scale 0.5 pigSkin)
      (Color.scale 0.1 pigSkin)
      (add pigSkin white |> Color.scale 1.1)

let eyes =
  importObj "../models/captain-toad/eyes.obj" identity eyeMat
let mouth =
  importObj "../models/captain-toad/mouth.obj" identity mouthMat
let hat =
  importObj "../models/captain-toad/hat.obj" identity hatMat
let scarf =
  importObj "../models/captain-toad/scarf.obj" identity scarfMat
let band =
  importObj "../models/captain-toad/band.obj" identity bandMat
let lamp =
  importObj "../models/captain-toad/lamp.obj" identity lampMat
let lightBulb =
  importObj "../models/captain-toad/light.obj" identity lightMat

let groupT = chain [
  rotateY (rad32 190.f)
  rotateZ (rad32 10.f)
  rotateX (rad32 -6.f)
  uniformScale 0.02f
]
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
  <| groupT
  <| defaultMaterial

let light = pointLight (point -7. 10. -8.) (color 1. 1. 0.9)
// let light = softLight (point -7. 10. -8.) (point 0. 0.7 0.) (color 1. 1. 0.9) 2 2.f
let cam =
  camera 300 200 (rad32 20.f)
  <| (point 0. 1. -14.) <| (point 0. 0.7 0.)
  
let w = 
  { world [light] [backDrop; toad] with
      ambientLight = Some ((Color.scale 0.1 blue), Add)
      shadows = true
      background = black
  }

let options =
  { defaultOptions with 
      antiAliasing = false
  }

let lightWorld =
  { w with objects = [group [lightBulb] groupT defaultMaterial]; shadows = false}
let mouthWorld =
  { w with objects = [group [mouth] groupT defaultMaterial]; shadows = false }

let FX image =
  let withAO =
    renderOcclusion defaultOptions 16 0.5f cam w
    |> applyOcclusion 0.8 (mix 0.3 black blue) image
  let mouthPass =
    renderImage defaultOptions cam mouthWorld
  let glow =
    renderImage defaultOptions cam lightWorld
    |> Blur.boxBlur 20

  Canvas.blendLayers Lighten mouthPass withAO
  |> Canvas.blendLayers Screen glow

let run () =
  render options cam w
