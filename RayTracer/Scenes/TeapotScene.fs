module TeapotScene

open System

open Camera
open Color
open Light
open Material
open ObjParser
open Shape
open Transform
open Tuple
open Util
open World


let wallMaterial = material red 0.1 0.9 0.

let floor = sphere (scale 10.f 0.01f 10.f) wallMaterial

let leftWall =
  sphere
  <| chain [
    translateZ 5.f
    rotateY (-MathF.PI / 5.3f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let rightWall =
  sphere
  <| chain [
    translateZ 6.f
    rotateY (MathF.PI / 3.f)
    rotateX (MathF.PI / 2.f)
    scale 10.f 0.01f 10.f
  ]
  <| wallMaterial

let sphereMat = Fresnel {
  a = materialSharp white 0.1 0.9 0.9
  b = Reflective None
  blend = Add
  power = 2.5
  mixOuter = 1.
  mixInner = 1.
}

let teapot =
  importObj "../models/teapot-mid.obj"
  <| chain [translateZ 1.5f; uniformScale 0.1f; rotateY (rad32 30.f)]
  <| sphereMat

let light = softLight (point -10. 10. -10.) (point 0. 1. 0.) (color 1. 1. 0.9) 6 8.f
let cam =
  camera 400 267 (rad32 47.f)
  <| (point 0. 1.5 -4.5) <| (point 0. 1. 0.)
  

let w = 
  ambientWorld
  <| Some (color 0. 0.15 0.3, Lighten)
  <| [light]
  <| [teapot; floor; leftWall; rightWall;]

let options =
  { defaultOptions with 
      antiAliasing = true }

let run () =
  render options cam w
