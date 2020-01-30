module Canvas

open System
open Color
open Util

type Canvas = Color [][]
let canvas w h =
  Array.init h (fun _ -> Array.init w (always black))

let width (c: Canvas) = Array.length c.[0]

let height (c: Canvas) = Array.length c

let length (c: Canvas) = (height c) * (width c)

let write x y (color: Color) (canvas: Canvas) =
  match (get2d x y canvas) with
  | Some (row, col) -> replace y (replace x color row) canvas
  | None -> canvas

let read x y (canvas: Canvas) =
  match(get2d x y canvas) with
  | Some (row, col) -> Some col
  | None -> None

let flatten a =
  Array.indexed a
  |> Array.fold (fun acc (y, row) ->
    let r =
      Array.indexed row
      |> Array.map (fun (x, color) -> (x, y, color))
    Array.concat [acc; r]
  ) Array.empty

// TODO: Rename to init
let map fn (a: Canvas) =
  a
  |> flatten
  |> Array.Parallel.map (fun (x, y, _) -> fn x y)
  |> Array.chunkBySize (width a)

// TODO: Rename to map
let mapC fn (a: Canvas) =
  a
  |> flatten
  |> Array.Parallel.map (fun (x, y, c) -> fn c)
  |> Array.chunkBySize (width a)

// TODO: Remove?
let render (fn: int -> int -> Color) (a: Canvas) =
  let pixels =
    a
    |> flatten
    |> Array.Parallel.map (fun (x, y, _) -> (x, y, fn x y))
  for (x, y, result) in pixels do
    a.[y].[x] <- result
  a

let blendLayers mode a b =
  map2d2 (blend mode) a b

let to256 = clamp 0.0 1.0 >> (*) 255.0 >> Math.Round

let ppmRow = Array.map (Color.toString to256 " ") >> String.concat " "
let ppmHeader w h = [|"P3"; String.Format ("{0} {1}", w, h); "255"|]
let toPpm (c: Canvas) =
  let header = ppmHeader (width c) (height c)
  let body = Array.map ppmRow c
  Array.concat [| header; body; [|""|] |]
