module Matrix

open Util

type Matrix (a: _[][]) =
  member x.Return = a

  static member Flatten (m: Matrix) =
    Array.reduce concat m.Return

  static member Map fn (m: Matrix) = 
    Array.map (Array.map fn) m.Return |> Matrix

  static member Mapi fn (m: Matrix) =
    m.Return |> Array.mapi (fun rowI row ->
      row |> Array.mapi (fun colI col -> fn rowI colI)
    ) |> Matrix

  static member (*) (mA: Matrix, mB: Matrix) =
    let a = mA.Return
    let b = mB.Return
    let widthA = Array.length a.[0]
    let widthB = Array.length b.[0]

    let mapper row col =
      let colB = min col (widthB - 1)
      [0..widthA-1]
      |> List.fold (fun acc i -> acc + a.[row].[i] * b.[i].[colB]) 0.0

    Array.create widthA (Array.create widthB 0.0) |> Matrix
    |> Matrix.Mapi mapper

  override x.GetHashCode () = x.GetHashCode ()
  override x.Equals (b: obj) =
    match b with
    | :? Matrix as m ->
      Array.indexed (Matrix.Flatten x)
      |> Array.forall (fun (i, el) -> looseEq el (Matrix.Flatten m).[i])
    | _ -> false

let matrix (a: float list list) =
  a |> List.toArray |> Array.map List.toArray |> Matrix

let create w h init =
  Array.create h (Array.create w init) |> Matrix

// TODO: Rename to 'set'?
let replace row col v (m: Matrix) =
  m.Return.[row].[col] <- v; m

let get row col (m: Matrix) =
  m.Return.[row].[col]

let identity () =
  create 4 4 0.0
  |> Matrix.Mapi (fun row col -> if (row = col) then 1.0 else 0.0)

let getColumn i (m: Matrix) =
  let folder acc (row: 'a[]) = appendTo acc row.[i]
  let res = Array.fold folder Array.empty m.Return
  res

let toTuple a =
  match (getColumn 0 a) with
  | [| x; y; z; w |] -> Tuple.Tuple (x, y, z, w)
  | _ -> failwith "Matrix must be 4D"

let multiply (a: Matrix) = (*) a

let multiplyT a t =
  Tuple.toArray t |> Matrix |> (*) a |> toTuple

let transpose (a: Matrix) =
  let l = (Array.length a.Return) - 1
  [|0..l|]
  |> Array.map (flip getColumn a) |> Matrix


let submatrix row col (m: Matrix) =
  filteri ((<>) row)
  >> Array.map (filteri ((<>) col)) <| m.Return
  |> Matrix

let rec determinant (m: Matrix) =
  let ma = m.Return
  if (Array.length ma <= 2)
  then
    match (ma.[0], ma.[1]) with
    | ([| a; b; |], [| c; d; |]) -> (a * d) - (b * c)
    | _ -> failwith "Must be a matrix of at least 2x2"
  else
    ma.[0]
    |> foldi (fun acc i _ ->
      acc + ma.[0].[i] * cofactor 0 i m
    ) 0.

and cofactor row col=
  let factor = if (isEven (row + col)) then 1.0 else -1.0
  submatrix row col >> determinant >> (*) factor

let minor row col = submatrix row col >> determinant

let invertible (m: Matrix) = (determinant m) <> 0.

let inverse (m: Matrix) =
  Matrix.Mapi (fun row col -> cofactor row col m) m
  |> transpose
  |> Matrix.Map (flip (/) (determinant m))
