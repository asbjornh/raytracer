module PatternTest

open Expecto

open Color
open Pattern
open Tuple

[<Tests>]
let tests =
  testList "Tests for Pattern" [
    testCase "Creating a stripe pattern" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal pattern.a white ""
      Expect.equal pattern.b black ""

    testCase "A stripe pattern is constant in y" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (stripeAt (point 0. 0. 0.) pattern) white ""
      Expect.equal (stripeAt (point 0. 1. 0.) pattern) white ""
      Expect.equal (stripeAt (point 0. 2. 0.) pattern) white ""

    testCase "A stripe pattern is constant in z" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (stripeAt (point 0. 0. 0.) pattern) white ""
      Expect.equal (stripeAt (point 0. 0. 1.) pattern) white ""
      Expect.equal (stripeAt (point 0. 0. 2.) pattern) white ""

    testCase "A stripe pattern alternates in x" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (stripeAt (point 0. 0. 0.) pattern) white "One"
      Expect.equal (stripeAt (point 0.9 0. 0.) pattern) white "Two"
      Expect.equal (stripeAt (point 1. 0. 0.) pattern) black "Three"
      Expect.equal (stripeAt (point -0.1 0. 0.) pattern) black "Four"
      Expect.equal (stripeAt (point -1. 0. 0.) pattern) black "Five"
      Expect.equal (stripeAt (point -1.1 0. 0.) pattern) white "Six"
  ]
