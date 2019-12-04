module Matrix

type Matrix = float [] []

let matrix (values: (float list) list): Matrix =
  List.map (List.toArray) values |> List.toArray

let equals (a: Matrix) (b: Matrix) = a = b
