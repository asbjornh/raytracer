module Tuple

open Util

type Tuple = float * float * float * float
let point x y z = Tuple (x, y, z, 1.0)

let vector x y z = Tuple (x, y, z, 0.0)

let epsilon = 0.00001
let equals a b = abs (a - b) < epsilon

let comnine op (a: Tuple) (b: Tuple) =
  let (x1, y1, z1, w1) = a
  let (x2, y2, z2, w2) = b;
  Tuple (op x1 x2, op y1 y2, op z1 z2, op w1 w2)

let map fn t =
  let (x, y, z, w) = t;
  Tuple (fn x, fn y, fn z, fn w)


let add = comnine (+)

let subtract = comnine (-)

let zero = vector 0.0 0.0 0.0
let negate = comnine (-) zero

let multiply n = map ((*) n)

let divide n = map (flip (/) n)

let sum (x, y, z, w) = x + y + z + w
let magnitude =
  map (pow 2.0) >> sum >> sqrt

let normalize (a: Tuple) =
  divide (magnitude a) a
