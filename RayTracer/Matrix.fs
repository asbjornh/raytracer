module Matrix

open Util

let matrix (a: float list list) =
  a |>List.toArray |> Array.map List.toArray

let create w h init =
  Array.create h (Array.create w init)

let replace row col v (m: 'a[][]) =
  m.[row].[col] <- v; m

let flatten (a: 'a[][]) =
  Array.reduce concat a

let equals (a: float [] []) (b: float [] []) =
  Array.indexed (flatten a)
  |> Array.forall (fun (i, el) -> looseEq el (flatten b).[i])

let map fn = Array.map (Array.map fn)

let mapi fn (a: float [] []) =
  a |> Array.mapi (fun rowI row ->
    row |> Array.mapi (fun colI col -> fn rowI colI)
  )

let identity () =
  create 4 4 0.0
  |> mapi (fun row col -> if (row = col) then 1.0 else 0.0)

let getColumn i =
  let folder acc (row: 'a[]) = appendTo acc row.[i]
  Array.fold folder Array.empty

let toTuple a =
  match (getColumn 0 a) with
  | [| x; y; z; w |] -> (x, y, z, w)
  | _ -> failwith "Matrix must be 4D"

let multiply (a: float [] []) (b: float [] []) =
  let widthA = Array.length a.[0]
  let widthB = Array.length b.[0]

  let mapper row col =
    let colB = min col (widthB - 1)
    [0..widthA-1]
    |> List.fold (fun acc i -> acc + a.[row].[i] * b.[i].[colB]) 0.0

  create widthB widthA 0.0
  |> mapi mapper

let multiplyTuple a b =
  Tuple.toMatrix b |> multiply a |> toTuple

let transpose (a: float [] []) =
  let l = (Array.length a) - 1
  [|0..l|]
  |> Array.map (flip getColumn a)


let submatrix row col =
  filteri ((<>) row)
  >> Array.map (filteri ((<>) col))

let rec determinant (m: float [] []) =
  if (Array.length m <= 2)
  then
    match (m.[0], m.[1]) with
    | ([| a; b; |], [| c; d; |]) -> (a * d) - (b * c)
    | _ -> failwith "Must be a matrix of at least 2x2"
  else
    m.[0]
    |> foldi (fun acc i _ ->
      acc + m.[0].[i] * cofactor 0 i m
    ) 0.

and cofactor row col =
  let factor = if (isEven (row + col)) then 1.0 else -1.0
  submatrix row col >> determinant >> (*) factor

let minor row col = submatrix row col >> determinant

let invertible = determinant >> (<>) 0.

let inverse a =
  mapi (fun row col -> cofactor row col a) a
  |> transpose
  |> map (flip (/) (determinant a))

let translation x y z =
  identity () |> replace 0 3 x |> replace 1 3 y |> replace 2 3 z
