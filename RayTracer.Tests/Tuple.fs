module TupleTest

open Expecto
open Tuple

[<Tests>]

let tests =
  testList "Tests for Tuple" [
    testCase "point() creates tuples with w=1" <| fun _ ->
      let p = point 4.0 -4.0 3.0 
      Expect.equal p (Tuple (4.0, -4.0, 3.0, 1.0)) ""

    testCase "vector() creates tuples with w=0" <| fun _ ->
      let v = vector 4.0 -4.0 3.0
      Expect.equal v (Tuple (4.0, -4.0, 3.0, 0.0) ) ""

    testCase "Adding two tuples" <| fun _ ->
      let a = Tuple (3.0, -2.0, 5.0, 1.0)
      let b = Tuple (-2.0, 3.0, 1.0, 0.0)
      Expect.equal (a + b) (Tuple (1.0, 1.0, 6.0, 1.0)) ""

    testCase "Subtracting two points" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = point 5.0 6.0 7.0
      Expect.equal (a - b) (vector -2.0 -4.0 -6.0) ""

    testCase "Subtracting a vector from a point" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      Expect.equal (a - b) (point -2.0 -4.0 -6.0) ""

    testCase "Subtracting two vectors" <| fun _ ->
      let a = vector 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      Expect.equal (a - b) (vector -2.0 -4.0 -6.0) ""

    testCase "Subtracting a vector from the zero vector" <| fun _ ->
      let zero = vector 0.0 0.0 0.0
      let a = vector 1.0 -2.0 3.0
      Expect.equal (zero - a) (vector -1.0 2.0 -3.0) ""

    testCase "Negating a tuple" <| fun _ ->
      let a = Tuple (1.0, -2.0, 3.0, -4.0)
      Expect.equal (negate a) (Tuple (-1.0, 2.0, -3.0, 4.0)) ""

    testCase "Multiplying a tuple by a scalar" <| fun _ ->
      let a = Tuple (1.0, -2.0, 3.0, -4.0)
      Expect.equal (3.5 * a) (Tuple (3.5, -7.0, 10.5, -14.0) ) ""

    testCase "Multiplying a tuple by a fraction" <| fun _ ->
      let a = Tuple (1.0, -2.0, 3.0, -4.0)
      Expect.equal (0.5 * a) (Tuple (0.5, -1.0, 1.5, -2.0)) ""

    testCase "Dividing a tuple by a scalar" <| fun _ ->
      let a = Tuple (1.0, -2.0, 3.0, -4.0)
      Expect.equal (2.0 / a) (Tuple (0.5, -1.0, 1.5, -2.0)) ""

    testCase "Computing the magnitude of vector(1, 0, 0)" <| fun _ ->
      let a = vector 1.0 0.0 0.0
      Expect.equal (magnitude a) 1.0 ""

    testCase "Computing the magnitude of vector(0, 1, 0)" <| fun _ ->
      let a = vector 0.0 1.0 0.0
      Expect.equal (magnitude a) 1.0 ""

    testCase "Computing the magnitude of vector(0, 0, 1)" <| fun _ ->
      let a = vector 0.0 0.0 1.0
      Expect.equal (magnitude a) 1.0 ""

    testCase "Computing the magnitude of vector(1, 2, 3)" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      Expect.equal (magnitude a) (sqrt 14.0) ""

    testCase "Computing the magnitude of vector(-1, -2, -3)" <| fun _ ->
      let a = vector -1.0 -2.0 -3.0
      Expect.equal (magnitude a) (sqrt 14.0) ""

    testCase "Normalizing vector(4, 0, 0) gives (1, 0, 0)" <| fun _ ->
      let a = vector 4.0 0.0 0.0
      Expect.equal (normalize a) (vector 1.0 0.0 0.0) ""

    testCase "Normalizing vector(1, 2, 3)" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let m = sqrt 14.0
      Expect.equal (normalize a) (vector (1.0 / m) (2.0 / m) (3.0 / m)) ""

    testCase "The magnitude of a normalized vector" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let norm = normalize a
      Expect.equal (magnitude norm) 1.0 ""

    testCase "The dot product of two tuples" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let b = vector 2.0 3.0 4.0
      Expect.equal (dot a b) 20.0 ""

    testCase "The cross product of two vectors" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let b = vector 2.0 3.0 4.0
      Expect.equal (cross a b) (vector -1.0 2.0 -1.0) ""
      Expect.equal (cross b a) (vector 1.0 -2.0 1.0) ""

    testCase "Reflecting a vector approaching at 45°" <| fun _ ->
      let v = vector 1. -1. 0.
      let n = vector 0. 1. 0.
      let r = reflect n v
      Expect.equal r (vector 1. 1. 0.) ""

    testCase "Reflecting a vector off a slanted surface" <| fun _ ->
      let a = (sqrt 2.) / 2.
      let v = vector 0. -1. 0.
      let n = vector a a 0.
      let r = reflect n v
      Expect.equal r (vector 1. 0. 0.) ""
  ]
