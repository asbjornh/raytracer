module Texture

open System.Drawing

open Color
open Tuple
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
  else (max - n) % max

let colorAt (p: Tuple) (cs: Color list list) =
  let w = List.length cs.[0]
  let h = List.length cs
  let x = wrapAround w (int p.X)
  let y = wrapAround h (int p.Z)
  cs.[y].[x]
