module Light

open Color
open Tuple

type Light = { intensity: Color; position: Tuple }

let pointLight position intensity =
  { intensity = intensity; position = position }
