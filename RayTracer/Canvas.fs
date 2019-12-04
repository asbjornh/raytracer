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

let write (canvas: Canvas) x y (color: Color) =
  match (get canvas y) with
  | Some (rowI, row) ->
    match (get row x) with
    | Some (colI, col) -> replace rowI (replace colI color row) canvas
    | None -> canvas
  | None -> canvas

let read (canvas: Canvas) x y =
  match (get canvas y) with
  | Some (rowI, row) ->
    match (get row x) with
    | Some (colI, col) -> Some col
    | None -> None
  | None -> None
