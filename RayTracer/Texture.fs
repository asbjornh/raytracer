module Texture

open System
open System.Numerics
open System.Drawing

open Color
open Tuple
open Transform
open Util

let toColor = float >> rangeMap (0., 255.) (0., 1.)

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


let colorAt (u: float32) (v: float32) (uScale, vScale, uOffset, vOffset) (cs: Color list list) =
  let u2 = float u / uScale + uOffset
  let v2 = float v / vScale + vOffset
  let w = List.length cs.[0]
  let h = List.length cs
  let x = Math.Floor (u2 * float w) |> int |> wrapAround (w - 1)
  let y = Math.Floor (v2 * float h) |> int |> wrapAround (h - 1)
  cs.[y].[x]

let mappedNormalAt (normalV: Vector4) (color: Color) =
  let ta = cross (vector 0. 1. 0.) (normalV)
  let bi = cross normalV ta
  let n = normalV
  let (r, g, b) = color.Return

  let t = 
    Matrix4x4 (
      -ta.X, bi.X, n.X, 0.f,
      ta.Y, bi.Y, n.Y, 0.f,
      -ta.Z, bi.Z, n.Z, 0.f,
      0.f, 0.f, 0.f, 1.f
    )
  transform t (vector r g b) |> normalize
