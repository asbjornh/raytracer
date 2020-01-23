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

let uvTransform = (0.25, 0.4, 0., 0.)
let baseMat = Textured {
  ambient = 0.2
  alpha = None
  color = Texture.read "../tex/padded-fabric-color.jpg"
  diffuse = 0.9
  specularMap = Some <| Texture.read "../tex/padded-fabric-specular.jpg"
  specular = 0.5
  shininess = 3.
  transform = i
  uvTransform = uvTransform
}

let mat = NormalMap {
  mat = baseMat
  tex = Texture.read "../tex/padded-fabric-normal.jpg"
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
