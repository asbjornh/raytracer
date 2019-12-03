module Units

type Tuple = float * float * float * float
let point x y z = Tuple (x, y, z, 1.0)

let vector x y z = Tuple (x, y, z, 0.0)

let epsilon = 0.00001
let equals a b = abs (a - b) < epsilon

let tupleTupleOp op (a: Tuple) (b: Tuple) =
  let (x1, y1, z1, w1) = a
  let (x2, y2, z2, w2) = b;
  Tuple (op x1 x2, op y1 y2, op z1 z2, op w1 w2)

let tupleScalarOp op (a: float) (b: Tuple) =
  let (x, y, z, w) = b;
  Tuple (op x a, op y a, op z a, op w a)


let zero = vector 0.0 0.0 0.0

let add (a: Tuple) (b: Tuple) =
  tupleTupleOp (+) a b

let subtract (a: Tuple) (b: Tuple) =
  tupleTupleOp (-) a b

let negate (a: Tuple) =
  tupleTupleOp (-) zero a

let multiply (a: float) (b: Tuple) =
  tupleScalarOp (*) a b

let divide (a: float) (b: Tuple) =
  tupleScalarOp (/) a b

let magnitude (a: Tuple) =
  let (x, y, z, _) = a
  sqrt (x ** 2.0 + y ** 2.0 + z ** 2.0)
