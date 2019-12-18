module PatternTest

open Expecto

open Color
open Matrix
open Pattern
open Shape
open Transform
open Tuple

let i = identity ()

[<Tests>]
let tests =
  testList "Tests for Pattern" [
    testCase "Creating a stripe pattern" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal pattern.A white ""
      Expect.equal pattern.B black ""

    testCase "A stripe pattern is constant in y" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 1. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 2. 0.) i pattern) white ""

    testCase "A stripe pattern is constant in z" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 0. 1.) i pattern) white ""
      Expect.equal (patternAt (point 0. 0. 2.) i pattern) white ""

    testCase "A stripe pattern alternates in x" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white "One"
      Expect.equal (patternAt (point 0.9 0. 0.) i pattern) white "Two"
      Expect.equal (patternAt (point 1. 0. 0.) i pattern) black "Three"
      Expect.equal (patternAt (point -0.1 0. 0.) i pattern) black "Four"
      Expect.equal (patternAt (point -1. 0. 0.) i pattern) black "Five"
      Expect.equal (patternAt (point -1.1 0. 0.) i pattern) white "Six"

    testCase "Stripes with an object transformation" <| fun _ ->
      let t = scaling 2. 2. 2.
      let object = sphereT t
      let pattern = stripePattern white black
      let c = patternAt (point 1.5 0. 0.) object.Transform pattern
      Expect.equal c white ""

    testCase "Stripes with a pattern transformation" <| fun _ ->
      let object = unitSphere ()
      let pattern = stripePatternT white black (scaling 2. 2. 2.)
      let c = patternAt (point 1.5 0. 0.) object.Transform pattern
      Expect.equal c white ""

    testCase "Stripes with both an object and a pattern transformation" <| fun _ ->
      let t = scaling 2. 2. 2.
      let object = sphereT t
      let pattern = stripePatternT white black (translation 0.5 0. 0.)
      let c = patternAt (point 2.5 0. 0.) object.Transform pattern
      Expect.equal c white ""

    testCase "A gradient linearly interpolates between colors" <| fun _ ->
      let pattern = gradientPattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0.25 0. 0.) i pattern) (color 0.75 0.75 0.75) ""
      Expect.equal (patternAt (point 0.5 0. 0.) i pattern) (color 0.5 0.5 0.5) ""
      Expect.equal (patternAt (point 0.75 0. 0.) i pattern) (color 0.25 0.25 0.25) ""

    testCase "A ring should extend in both x and z" <| fun _ ->
      let pattern = ringPattern  white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 1. 0. 0.) i pattern) black ""
      Expect.equal (patternAt (point 0. 0. 1.) i pattern) black ""
      // 0.708 = just slightly more than √2/2​
      Expect.equal (patternAt (point 0.708 0. 0.708) i pattern) black ""

    testCase "Checkers should repeat in x" <| fun _ ->
      let pattern = checkersPattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0.99 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 1.01 0. 0.) i pattern) black ""

    testCase "Checkers should repeat in y" <| fun _ ->
      let pattern = checkersPattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 0.99 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 1.01 0.) i pattern) black ""

    testCase "Checkers should repeat in z" <| fun _ ->
      let pattern = checkersPattern white black
      Expect.equal (patternAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (patternAt (point 0. 0. 0.99) i pattern) white ""
      Expect.equal (patternAt (point 0. 0. 1.01) i pattern) black ""
  ]
