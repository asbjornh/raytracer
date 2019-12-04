module Canvas

open System
open Color
open Util

type Canvas = (Color list) list
let canvas w h =
  List.init h (fun _ -> List.init w (fun _ -> color 0.0 0.0 0.0))

let width (c: Canvas) =
  match c with
  | first :: rest -> List.length first
  | _ -> 0

let height (c: Canvas) = List.length c

let write x y (color: Color) (canvas: Canvas) =
  match (get2d x y canvas) with
  | Some (row, col) -> replace y (replace x color row) canvas
  | None -> canvas

let read x y (canvas: Canvas) =
  match(get2d x y canvas) with
  | Some (row, col) -> Some col
  | None -> None

let ppmHeader w h = String.Format ("P3\n{0} {1}\n255", w, h)

let clamp lower upper n = n |> (min upper) |> (max lower)
let colorTo255 (c: Color) =
  c
  |> Color.map (clamp 0.0 1.0 >> (*) 255.0 >> Math.Round >> toString)
  |> Color.toList
  |> String.concat " "

let ppmRow = List.map colorTo255 >> String.concat " "
let ppmBody = List.map ppmRow >> String.concat "\n"
let toPpm (c: Canvas) = (ppmHeader (width c) (height c)) + "\n" + (ppmBody c)