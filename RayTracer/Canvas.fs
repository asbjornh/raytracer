module Canvas

open Color

type Canvas = (Color list) list
let canvas w h =
  List.init h (fun _ -> List.init w (fun _ -> color 0.0 0.0 0.0))

let width (c: Canvas) =
  match c with
  | first :: rest -> List.length first
  | _ -> 0

let height (c: Canvas) = List.length c