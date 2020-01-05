module Light

open Color
open Matrix
open Tuple
open Transform
open Util

type PointLight = { intensity: Color; position: Tuple }
type ConstantLight = { intensity: Color; mode: BlendingMode }
type SoftLight = { light: PointLight; virtualLights: Tuple list }

type Light =
  | PointLight of PointLight
  | ConstantLight of ConstantLight
  | SoftLight of SoftLight

let pointLightFactory position intensity =
  { intensity = intensity; position = position }

let pointLight p i =
  pointLightFactory p i |> PointLight

let constantLight intensity mode =
  ConstantLight { intensity = intensity; mode = mode }

let ringLight (position: Tuple) target intensity count spread =
  let up = vector 0. 0. 1.
  List.init count (fun i ->
    let degrees = 360.f / (float32 count) * (float32 i)
    let transform = chain [
      translate position.X position.Y position.Z
      lookAt position target up
      rotateZ (rad32 degrees)
      translateY spread
    ]
    let p = multiplyT transform (point 0. 0. 0.)
    let i = Color.scale (1. / float count) intensity
    pointLight p i
  )

let squareOfPoints (position: Tuple) target resolution size =
  let up = vector 0. 0. 1.
  List.init resolution (fun y ->
    List.init resolution (fun x ->
      let delta = size / (float32 resolution)
      let offset = size / 4.f
      let transform = chain [
        translate position.X position.Y position.Z
        lookAt position target up
        translate (float32 x * delta - offset) (float32 y * delta - offset) 0.f
      ]
      multiplyT transform (point 0. 0. 0.)
    )
  )
  |> List.reduce (fun a b -> List.concat [a; b])

let softLight (position: Tuple) direction intensity resolution size =
  let points = squareOfPoints position direction resolution size
  let l = { position = position; intensity = intensity; }
  SoftLight { light = l; virtualLights = points }

let squareLight (position: Tuple) direction intensity resolution size =
  let count = resolution * resolution
  let i = Color.scale (1. / float count) intensity

  squareOfPoints position direction resolution size
    |> List.map (fun p -> pointLight p i)
