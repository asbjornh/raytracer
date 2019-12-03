module Units

type Tuple = float * float * float * float
let point x y z = Tuple (x, y, z, 1.0)

let vector x y z = Tuple (x, y, z, 0.0)

let epsilon = 0.00001
let equals a b = abs (a - b) < epsilon

let tupleOp op (a: Tuple) (b: Tuple) =
  let (x1, y1, z1, w1) = a
  let (x2, y2, z2, w2) = b;
  Tuple (op x1 x2, op y1 y2, op z1 z2, op w1 w2)


let zero = vector 0.0 0.0 0.0
let inline (++) (a: Tuple) (b: Tuple) = tupleOp (+) a b
let inline (--) (a: Tuple) (b: Tuple) = tupleOp (-) a b
let inline (!!) (a: Tuple) = tupleOp (-) zero a

let t2 = (vector 1.0 1.0 1.0) -- (vector 1.0 1.0 1.0)