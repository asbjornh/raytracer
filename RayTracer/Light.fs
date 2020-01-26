module Light

open System.Numerics

open Color
open Matrix
open Tuple
open Transform
open Util

type PointLight = { intensity: Color; position: Vector4 }
type SoftLight = { light: PointLight; virtualLights: Vector4 list }

type Light =
  | PointLight of PointLight
  | SoftLight of SoftLight

let pointLightFactory position intensity =
  { intensity = intensity; position = position }

let pointLight p i =
  pointLightFactory p i |> PointLight

let ringLight (position: Vector4) target intensity count spread =
  let up = vector 0. 0. 1.
  List.init count (fun i ->
    let degrees = 360.f / (float32 count) * (float32 i)
    let t = chain [
      translate position.X position.Y position.Z
      lookAt position target up
      rotateZ (rad32 degrees)
      translateY spread
    ]
    let p = transform t (point 0. 0. 0.)
    let i = Color.scale (1. / float count) intensity
    pointLight p i
  )

let squareOfPoints (position: Vector4) target resolution size =
  let up = vector 0. 0. 1.
  List.init resolution (fun y ->
    List.init resolution (fun x ->
      let delta = size / (float32 resolution)
      let offset = size / 4.f
      let t = chain [
        translate position.X position.Y position.Z
        lookAt position target up
        translate (float32 x * delta - offset) (float32 y * delta - offset) 0.f
      ]
      transform t (point 0. 0. 0.)
    )
  )
  |> List.reduce (fun a b -> List.concat [a; b])

let softLight position target intensity resolution size =
  let points = squareOfPoints position target resolution size
  let l = { position = position; intensity = intensity; }
  SoftLight { light = l; virtualLights = points }

let squareLight position target intensity resolution size =
  let count = resolution * resolution
  let i = Color.scale (1. / float count) intensity

  squareOfPoints position target resolution size
    |> List.map (fun p -> pointLight p i)
