module Textures

open System

open Camera
open Color
open Light
open Material
open Matrix
open Shape
open Transform
open Tuple
open World


let i = identity

let uvTransform = (0.4, 0.8, 0., 0.)
let baseMat = Textured {
  ambient = 0.6
  ambientOcclusion = None
  alpha = Some <| Texture.read "../tex/metal-gate-alpha.jpg"
  color = Texture.read "../tex/metal-gate-color.jpg"
  diffuse = 1.5
  specularMap = Some <| Texture.read "../tex/metal-gate-specular.jpg"
  specular = white
  shininess = 3.
  transform = i
  uvTransform = uvTransform
}

let mat = NormalMap {
  mat = baseMat
  tex = Texture.read "../tex/metal-gate-normal.jpg"
  transform = i
  uvTransform = uvTransform
}

let ball =
  sphere i mat

let pLight = pointLight (point -10. 7. 2.) (color 1. 0.9 0.7)
let pLight2 = pointLight (point 5. 0. -2.) (color 0.8 0.9 1.)
let cam = 
  camera 500 500 (MathF.PI / 3.f)
  <| (point 0. 0. -3.) <| (point 0. 0. 0.)

let w = {
  world [pLight; pLight2] [ball] with
    background = black
    shadows = false
}

let options =
  { defaultOptions with
      antiAliasing = false }

let run () =
  render options cam w
