module Light

open Color
open Matrix
open Tuple
open Transform
open Util

type PointLight = { intensity: Color; position: Tuple }
type ConstantLight = { intensity: Color; }

type Light =
  | PointLight of PointLight
  | ConstantLight of ConstantLight

let pointLightFactory position intensity =
  { intensity = intensity; position = position }

let pointLight p i =
  pointLightFactory p i |> PointLight

let constantLight intensity =
  ConstantLight { intensity = intensity; }

let ringLight (position: Tuple) direction intensity count spread =
  let up = vector 0. 0. 1.
  List.init count (fun i ->
    let degrees = 360. / (float count) * (float i)
    let transform = chain [
      translate position.X position.Y position.Z
      rotateAlign up direction
      rotateZ (rad degrees)
      translateY spread
    ]
    let p = multiplyT transform (point 0. 0. 0.)
    let i = Color.scale (1. / float count) intensity
    pointLight p i
  )

let squareLight (position: Tuple) direction intensity resolution spread =
  let up = vector 0. 0. 1.
  let count = resolution * resolution
  List.init resolution (fun y ->
    List.init resolution (fun x ->
      let transform = chain [
        translate position.X position.Y position.Z
        translate (float x * spread) (float y * spread) 0.
        rotateAlign up direction
      ]
      let p = multiplyT transform (point 0. 0. 0.)
      let i = Color.scale (1. / float count) intensity
      pointLight p i
    )
  )
  |> List.reduce (fun a b -> List.concat [a; b])
