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

let isIndex a (b, _) = a = b

let get list index =
  List.indexed list |> List.tryFind (isIndex index)

let replace index (newEl: 'a) (list: 'a list)=
  list |> List.mapi (fun i el ->
    if (i = index) then newEl else el
  )

let get2d x y (list: 'a list list) =
  match (get list y) with
  | Some (rowI, row) ->
    match (get row x) with
    | Some (colI, col) -> Some (row, col)
    | None -> None
  | None -> None

let write x y (color: Color) (canvas: Canvas) =
  match (get2d x y canvas) with
  | Some (row, col) -> replace y (replace x color row) canvas
  | None -> canvas

let read x y (canvas: Canvas) =
  match(get2d x y canvas) with
  | Some (row, col) -> Some col
  | None -> None
