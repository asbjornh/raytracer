module Matrix

let equals (a: float [,]) (b: float [,]) = a = b

let multiply (a: float [,]) (b: float [,]) =
  let size = Array2D.length1 a

  let map row col _ =
    [0..size-1]
    |> List.fold (fun acc i -> acc + a.[row, i] * b.[i, col]) 0.0

  Array2D.zeroCreate size size
  |> Array2D.mapi map