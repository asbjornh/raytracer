module TestUtils

open Expecto

open Tuple
open Util

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

let expectTupleEq actual expected =
  Expect.isTrue
  <| (equals actual expected)
  <| (diff actual expected)

let expectTupleEqTxt actual expected txt =
  Expect.isTrue
  <| (equals actual expected)
  <| txt + " " + (diff actual expected)

let expectFloatEq actual expected =
  Expect.isTrue
  <| looseEq32 actual expected
  <| diff actual expected
