module Util

open System

// Function helpers
let always a _ = a
let flip fn a b = fn b a

// Numbers
let epsilon = 0.00001
let looseEq a b = abs (a - b) < epsilon
let pow exponent num = num ** exponent
let clamp lower upper n = n |> (min upper) |> (max lower)
let rangeMap (inMin: float, inMax) (outMin, outMax) x
  = (x - inMin) / (inMax - inMin) * (outMax - outMin) + outMin
let isEven a = (a % 2) = 0
let rad deg = (deg / 180.) * Math.PI

// String
let toString a = a.ToString ()

// List
let isIndex a b = a = fst b
let get arr index =
  Array.indexed arr |> Array.tryFind (isIndex index)
let replace index (newEl: 'a) (list: 'a [])=
  list.[index] <- newEl; list

// Array
let concat a b = Array.concat [a; b]
let appendTo arr el = concat arr [| el |]
let filteri fn m =
  Array.indexed m
  |> Array.filter (fst >> fn)
  |> Array.map snd

let get2d x y (arr: 'a [][]) =
  try
    let row = arr.[y]
    let col = row.[x]
    Some (row, col)
  with | _ -> None

let foldi fn initial m =
  Array.indexed m
  |> Array.fold (fun acc (i, el) -> fn acc i el) initial

// IO
let writeFile (path: string) content =
  System.IO.File.WriteAllLines (path, content)
