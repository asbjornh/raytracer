module Light

open Color
open Tuple

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
