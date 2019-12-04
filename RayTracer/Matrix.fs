module Matrix

let equals (a: float [,]) (b: float [,]) = a = b

let multiply (a: float [,]) (b: float [,]) =
  let w = Array2D.length1 a
  let h = Array2D.length2 a
  let m = Array2D.zeroCreate w h

  for col in [0..h - 1] do
    for row in [0..w - 1] do
      let v = (
        a.[row, 0] * b.[0, col]
        + a.[row, 1] * b.[1, col]
        + a.[row, 2] * b.[2, col]
        + a.[row, 3] * b.[3, col]
      )
      m.[row, col] <- v
  m