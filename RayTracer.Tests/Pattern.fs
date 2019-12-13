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
      Expect.equal (colorAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (colorAt (point 0. 1. 0.) i pattern) white ""
      Expect.equal (colorAt (point 0. 2. 0.) i pattern) white ""

    testCase "A stripe pattern is constant in z" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (colorAt (point 0. 0. 0.) i pattern) white ""
      Expect.equal (colorAt (point 0. 0. 1.) i pattern) white ""
      Expect.equal (colorAt (point 0. 0. 2.) i pattern) white ""

    testCase "A stripe pattern alternates in x" <| fun _ ->
      let pattern = stripePattern white black
      Expect.equal (colorAt (point 0. 0. 0.) i pattern) white "One"
      Expect.equal (colorAt (point 0.9 0. 0.) i pattern) white "Two"
      Expect.equal (colorAt (point 1. 0. 0.) i pattern) black "Three"
      Expect.equal (colorAt (point -0.1 0. 0.) i pattern) black "Four"
      Expect.equal (colorAt (point -1. 0. 0.) i pattern) black "Five"
      Expect.equal (colorAt (point -1.1 0. 0.) i pattern) white "Six"

    testCase "Stripes with an object transformation" <| fun _ ->
      let t = scaling 2. 2. 2.
      let object = sphereT t
      let pattern = stripePattern white black
      let c = colorAt (point 1.5 0. 0.) object.Transform pattern
      Expect.equal c white ""

    testCase "Stripes with a pattern transformation" <| fun _ ->
      let object = unitSphere ()
      let pattern = stripePatternT white black (scaling 2. 2. 2.)
      let c = colorAt (point 1.5 0. 0.) object.Transform pattern
      Expect.equal c white ""

    testCase "Stripes with both an object and a pattern transformation" <| fun _ ->
      let t = scaling 2. 2. 2.
      let object = sphereT t
      let pattern = stripePatternT white black (translation 0.5 0. 0.)
      let c = colorAt (point 2.5 0. 0.) object.Transform pattern
      Expect.equal c white ""
  ]
