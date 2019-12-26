module Texture

open System
open System.Drawing

open Color
open Matrix
open Pattern
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


let colorAt u v uScale vScale uOffset vOffset (cs: Color list list) =
  let u2 = u / uScale + uOffset
  let v2 = v / vScale + vOffset
  let w = List.length cs.[0]
  let h = List.length cs
  let x = Math.Floor (u2 * float w) |> int |> wrapAround (w - 1)
  let y = Math.Floor (v2 * float h) |> int |> wrapAround (h - 1)
  cs.[y].[x]

let mappedNormalAt (normalV: Tuple) (color: Color) =
  let rotation = rotateAlign (vector 0. 1. 0.) normalV
  let t = multiplyT rotation (vector 0. 0. 1.)
  let b = multiplyT rotation (vector -1. 0. 0.)
  let n = normalV
  let transform = matrix [
    [ t.X ; b.X ; n.X ; 0. ]
    [ -t.Y; -b.Y; n.Y ; 0. ]
    [ t.Z ; b.Z ; n.Z ; 0. ]
    [ 0.  ; 0.  ; 0.  ; 1. ]
  ]
  let (r, g, b) = color.Return
  multiplyT transform <| vector r g b
