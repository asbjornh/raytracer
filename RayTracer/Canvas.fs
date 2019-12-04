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

let to256 = clamp 0.0 1.0 >> (*) 255.0 >> Math.Round

let ppmRow = List.map (Color.toString to256 " ") >> String.concat " "
let ppmBody = List.map ppmRow >> String.concat "\n"
let ppmHeader w h = String.Format ("P3\n{0} {1}\n255", w, h)
let toPpm (c: Canvas) =
  let header = ppmHeader (width c) (height c)
  let body = ppmBody c
  header + "\n" + body