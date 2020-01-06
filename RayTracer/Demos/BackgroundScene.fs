module BackgroundScene

open Camera
open Color
open Light
open ObjParser
open Matrix
open Material
open Shape
open Transform
open Tuple
open Util
open World


let bgMat =
  texture "../tex/hall.jpg"
  <| (1., 1.) <| (0., 0.)
  <| rotateX (rad32 5.f)
let background =
  sphere
  <| uniformScale 1000.f
  <| bgMat

let darkBlue = color 0.6 0.75 0.9 |> Color.scale 0.6
let floorMat = InvisFloor { shadowColor = darkBlue |> Color.scale 1.15 }
let floor =
  plane
  <| chain [translateY -10.f; uniformScale 40.f;]
  <| floorMat

let mat = Fresnel {
  a = material white 0.3 0.7 0.
  b = Reflective { blend = Lighten }
  power = 3.
  mixOuter = 0.4
  mixInner = 1.
}
let teapot =
  objFromFile "../models/teapot-mid.obj"
  <| chain [translate -2.f -6.f -5.f; rotateZ (rad32 20.f); rotateY (rad32 20.f)]
  <| mat

let lightBlue = mix white cyan 0.2
let sLight =
  softLight (point 0. 100. 100.) (point 0. 0. 0.) lightBlue 4 40.f
let cLight = constantLight (darkBlue |> Color.scale 0.5) Add
let pLight = pointLight (point 0. 100. -50.) (Color.mix blue white 0.4 |> Color.scale 0.3)
let cam = 
  camera 400 250 (rad32 60.f)
  <| (point 0. 10. -100.) <| (point 0. 1. 0.)

let w =
  world
  <| [pLight; sLight; cLight]
  <| [background; teapot; floor]


let run () =
  render defaultOptions cam w
  |> Util.writeFile ("../render/" + (Util.nowStr ()) + ".ppm")
