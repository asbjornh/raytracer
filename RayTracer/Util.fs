module Util

open System

// Function helpers
let always a _ = a
let flip fn a b = fn b a

// Numbers
let epsilon = 0.00001
let looseEq a b = abs (a - b) < epsilon
let pow a b = b ** a
let clamp lower upper n = n |> (min upper) |> (max lower)
let isEven a = (a % 2) = 0
let rad deg = (deg / 180.) * Math.PI

// String
let toString a = a.ToString ()

// List
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

// Array
let concat a b = Array.concat [a; b]
let appendTo arr el = concat arr [| el |]
let filteri fn m =
  Array.indexed m
  |> Array.filter (fst >> fn)
  |> Array.map snd

let foldi fn initial m =
  Array.indexed m
  |> Array.fold (fun acc (i, el) -> fn acc i el) initial

// IO
let writeFile (path: string) content =
  System.IO.File.WriteAllLines (path, List.toArray content)