module Texture

open System
open System.Numerics
open System.Drawing

open Color
open Tuple
open Transform
open Util

let toColor = float >> rangeMap (0., 255.) (0., 1.)

let solid color = [[color]]
let read path =
  let i = Image.FromFile path
  let b = new Bitmap (i)

  [0..b.Height - 1]
  |> List.mapi (fun y _ ->
    [0..b.Width - 1] |> List.mapi (fun x _ ->
      let c = b.GetPixel (x, y)
      color <| toColor c.R <| toColor c.G <| toColor c.B
    )
  )

let rec wrapAround max n =
  if n >= max then (n - max) % max
  else if (n >= 0) then n
  else ((n - max) % max) + max

let textureCoord (num: float) max =
  Math.Round num |> int |> wrapAround (max - 1)

let interpolatedColor (cs: Color list list) (u: float) (v: float) =
  let w = List.length cs.[0]
  let h = List.length cs
  let x = u * float w
  let y = v * float h
  let xRound = Math.Floor x
  let yRound = Math.Floor y
  let color = cs.[textureCoord y h].[textureCoord x w]
  let colorRound = cs.[textureCoord yRound h].[textureCoord xRound w]
  mix 0.5 colorRound color

let colorAt (u: float32) (v: float32) (uScale, vScale, uOffset, vOffset) (cs: Color list list) =
  if List.length cs = 1 then
    cs.[0].[0]
  else
    interpolatedColor cs
      (float u / uScale + uOffset)
      (float v / vScale + vOffset)

let normalFromColor (normalV: Vector4) (color: Color) =
  let t = cross (vector 0. 1. 0.) normalV // tangent
  let b = cross normalV t // bitangent
  let n = normalV
  let tangentToModel = 
    Matrix4x4 (
      -t.X, b.X, n.X, 0.f,
      t.Y, b.Y, n.Y, 0.f,
      -t.Z, b.Z, n.Z, 0.f,
      0.f, 0.f, 0.f, 1.f
    )
  let (r, g, b) = color |> Color.Map (rangeMap (0., 1.) (-1., 1.))
  transform tangentToModel (vector r g b) |> normalize
