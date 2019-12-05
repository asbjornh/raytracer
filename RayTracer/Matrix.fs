module Matrix

open Util

let matrix (a: float list list) =
  a |>List.toArray |> Array.map List.toArray

let create w h init =
  Array.create h (Array.create w init)

let equals (a: float [] []) (b: float [] []) = a = b

let map fn (a: float [] []) =
  a |> Array.map (fun row ->
    row |> Array.map (fun col -> fn row col)
  )

let mapi fn (a: float [] []) =
  a |> Array.mapi (fun rowI row ->
    row |> Array.mapi (fun colI col -> fn rowI colI col)
  )

let identity =
  create 4 4 0.0
  |> mapi (fun row col _ -> if (row = col) then 1.0 else 0.0)

let filteri fn m =
  Array.indexed m
  |> Array.filter (fun (i, el) -> fn i)
  |> Array.map (fun (i, el) -> el)

let getColumn i =
  let folder acc (row: float []) = Array.concat [ acc; [|row.[i]|] ]
  Array.fold folder Array.empty

let toTuple a =
  match (getColumn 0 a) with
  | [| x; y; z; w |] -> (x, y, z, w)
  | _ -> failwith "Matrix must be 4D"

let multiply (a: float [] []) (b: float [] []) =
  let widthA = Array.length a.[0]
  let widthB = Array.length b.[0]

  let map row col _ =
    let colB = min col (widthB - 1)
    [0..widthA-1]
    |> List.fold (fun acc i -> acc + a.[row].[i] * b.[i].[colB]) 0.0

  create widthB widthA 0.0
  |> mapi map

let multiplyTuple a b =
  Tuple.toMatrix b |> multiply a |> toTuple

let transpose (a: float [] []) =
  let l = (Array.length a) - 1
  [|0..l|]
  |> Array.map (flip getColumn a)

let determinant (m: float [] []) =
  match (m.[0], m.[1]) with
  | ([| a; b; |], [| c; d; |]) -> (a * d) - (b * c)
  | _ -> failwith "Must be a 2x2 matrix"

let submatrix row col =
  filteri ((<>) row)
  >> Array.map (filteri ((<>) col))

let minor row col = submatrix row col >> determinant

let cofactor row col =
  let factor = if (isEven (row + col)) then 1.0 else -1.0
  minor row col >> (*) factor