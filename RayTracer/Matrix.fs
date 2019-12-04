module Matrix

let equals (a: float [,]) (b: float [,]) = a = b

let multiply (a: float [,]) (b: float [,]) =
  let widthA = Array2D.length2 a
  let widthB = Array2D.length2 b

  let map row col _ =
    let colB = min col (widthB - 1)
    [0..widthA-1]
    |> List.fold (fun acc i -> acc + a.[row, i] * b.[i, colB]) 0.0

  Array2D.zeroCreate widthA widthB
  |> Array2D.mapi map

let flatten (a: 'a[,]) = a |> Seq.cast<'a>

let getColumn col (a: _[,]) =
    flatten a.[*,col..col] |> Seq.toArray

let getRow row (a: _[,]) =
    flatten a.[row..row,*] |> Seq.toArray  

let toTuple (a: float [,]) =
  match (getColumn 0 a) with
  | [| x; y; z; w |] -> (x, y, z, w)
  | _ -> failwith "Matrix must be 4D"