module Matrix

open Util

let identity =
  Array2D.zeroCreate 4 4
  |> Array2D.mapi (fun row col _ -> if (row = col) then 1.0 else 0.0)

let equals (a: float [,]) (b: float [,]) = a = b

let flatten (a: 'a[,]) = a |> Seq.cast<'a>

let getColumn col (a: _[,]) =
    flatten a.[*,col..col] |> Seq.toArray

let getRow row (a: _[,]) =
    flatten a.[row..row,*] |> Seq.toArray  

let toTuple (a: float [,]) =
  match (getColumn 0 a) with
  | [| x; y; z; w |] -> (x, y, z, w)
  | _ -> failwith "Matrix must be 4D"

let multiply (a: float [,]) (b: float [,]) =
  let widthA = Array2D.length2 a
  let widthB = Array2D.length2 b

  let map row col _ =
    let colB = min col (widthB - 1)
    [0..widthA-1]
    |> List.fold (fun acc i -> acc + a.[row, i] * b.[i, colB]) 0.0

  Array2D.zeroCreate widthA widthB
  |> Array2D.mapi map

let multiplyTuple a b =
  Tuple.toMatrix b |> multiply a |> toTuple

let transpose (a: float [,]) =
  let l = (Array2D.length1 a) - 1
  [|0..l|]
  |> Array.map (flip getColumn a)
  |> array2D

let determinant (m: float [,]) =
  match (getRow 0 m, getRow 1 m) with
  | ([| a; b; |], [| c; d; |]) -> (a * d) - (b * c)
  | _ -> failwith "Must be a 2x2 matrix"