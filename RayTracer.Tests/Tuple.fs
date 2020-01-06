module TupleTest

open System.Numerics

open Expecto

open TestUtils
open Tuple
open Util


[<Tests>]

let tests =
  testList "Tests for Tuple" [
    testCase "point() creates tuples with w=1" <| fun _ ->
      let p = point 4.0 -4.0 3.0 
      expectTupleEq p (Tuple (4.0f, -4.0f, 3.0f, 1.0f))

    testCase "vector() creates tuples with w=0" <| fun _ ->
      let v = vector 4.0 -4.0 3.0
      expectTupleEq v (Tuple (4.0f, -4.0f, 3.0f, 0.0f) )

    testCase "Adding two tuples" <| fun _ ->
      let a = Tuple (3.0f, -2.0f, 5.0f, 1.0f)
      let b = Tuple (-2.0f, 3.0f, 1.0f, 0.0f)
      expectTupleEq (a + b) (Tuple (1.0f, 1.0f, 6.0f, 1.0f))

    testCase "Subtracting two points" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = point 5.0 6.0 7.0
      expectTupleEq (a - b) (vector -2.0 -4.0 -6.0)

    testCase "Subtracting a vector from a point" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      expectTupleEq (a - b) (point -2.0 -4.0 -6.0)

    testCase "Subtracting two vectors" <| fun _ ->
      let a = vector 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      expectTupleEq (a - b) (vector -2.0 -4.0 -6.0)

    testCase "Subtracting a vector from the zero vector" <| fun _ ->
      let zero = vector 0.0 0.0 0.0
      let a = vector 1.0 -2.0 3.0
      expectTupleEq (zero - a) (vector -1.0 2.0 -3.0)

    testCase "Negating a tuple" <| fun _ ->
      let a = Tuple (1.0f, -2.0f, 3.0f, -4.0f)
      expectTupleEq (negate a) (Tuple (-1.0f, 2.0f, -3.0f, 4.0f))

    testCase "Multiplying a tuple by a scalar" <| fun _ ->
      let a = Tuple (1.0f, -2.0f, 3.0f, -4.0f)
      expectTupleEq (3.5f * a) (Tuple (3.5f, -7.0f, 10.5f, -14.0f) )

    testCase "Multiplying a tuple by a fraction" <| fun _ ->
      let a = Tuple (1.0f, -2.0f, 3.0f, -4.0f)
      expectTupleEq (0.5f * a) (Tuple (0.5f, -1.0f, 1.5f, -2.0f))

    testCase "Dividing a tuple by a scalar" <| fun _ ->
      let a = Tuple (1.0f, -2.0f, 3.0f, -4.0f)
      expectTupleEq (2.0f / a) (Tuple (0.5f, -1.0f, 1.5f, -2.0f))

    testCase "Magnitude is the same as distance between the base and tip of a vector" <| fun _ ->
      let a = point32 1.f 0.f 0.f
      let b = point32 0.f 1.f 1.f
      let m = magnitude (a - b)
      let v = Vector4.Distance (a.Vec, b.Vec)
      Expect.equal m v ""

    testCase "Computing the magnitude of vector(1, 0, 0)" <| fun _ ->
      let a = vector 1.0 0.0 0.0
      Expect.equal (magnitude a) 1.0f ""

    testCase "Computing the magnitude of vector(0, 1, 0)" <| fun _ ->
      let a = vector 0.0 1.0 0.0
      Expect.equal (magnitude a) 1.0f ""

    testCase "Computing the magnitude of vector(0, 0, 1)" <| fun _ ->
      let a = vector 0.0 0.0 1.0
      Expect.equal (magnitude a) 1.0f ""

    testCase "Computing the magnitude of vector(1, 2, 3)" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      Expect.equal (magnitude a) (sqrt 14.0f) ""

    testCase "Computing the magnitude of vector(-1, -2, -3)" <| fun _ ->
      let a = vector -1.0 -2.0 -3.0
      Expect.equal (magnitude a) (sqrt 14.0f) ""

    testCase "Normalizing vector(4, 0, 0) gives (1, 0, 0)" <| fun _ ->
      let a = vector 4.0 0.0 0.0
      expectTupleEq (normalize a) (vector 1.0 0.0 0.0)

    testCase "Normalizing vector(1, 2, 3)" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let m = sqrt 14.0
      expectTupleEq (normalize a) (vector (1.0 / m) (2.0 / m) (3.0 / m))

    testCase "The magnitude of a normalized vector" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let norm = normalize a
      Expect.isTrue <| looseEq32 (magnitude norm) 1.0f <| ""

    testCase "The dot product of two tuples" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let b = vector 2.0 3.0 4.0
      Expect.equal (dot a b) 20.0f ""

    testCase "The cross product of two vectors" <| fun _ ->
      let a = vector 1.0 2.0 3.0
      let b = vector 2.0 3.0 4.0
      expectTupleEq (cross a b) (vector -1.0 2.0 -1.0)
      expectTupleEq (cross b a) (vector 1.0 -2.0 1.0)

    testCase "Reflecting a vector approaching at 45°" <| fun _ ->
      let v = vector 1. -1. 0.
      let n = vector 0. 1. 0.
      let r = reflect n v
      expectTupleEq r (vector 1. 1. 0.)

    testCase "Reflecting a vector off a slanted surface" <| fun _ ->
      let a = (sqrt 2.) / 2.
      let v = vector 0. -1. 0.
      let n = vector a a 0.
      let r = reflect n v
      expectTupleEq r (vector 1. 0. 0.)
  ]
