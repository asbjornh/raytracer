module Util

let pow a b = b ** a

let clamp lower upper n = n |> (min upper) |> (max lower)

let flip fn a b = fn b a

let toString a = a.ToString ()

let isIndex a b = a = fst b
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


let writeFile (path: string) content =
  System.IO.File.WriteAllLines (path, List.toArray content)