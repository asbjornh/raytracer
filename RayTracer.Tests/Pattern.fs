module PatternTest

open Expecto

open Color
open Material
open Matrix
open Pattern
open Shape
open Transform
open Tuple

let i = identity ()

[<Tests>]
let tests =
  testList "Tests for Pattern" [
    testCase "A stripe pattern is constant in y" <| fun _ ->
      let pattern = patternAt white black Stripes
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0. 1. 0.)) white ""
      Expect.equal (pattern (point 0. 2. 0.)) white ""

    testCase "A stripe pattern is constant in z" <| fun _ ->
      let pattern = patternAt white black Stripes
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0. 0. 1.)) white ""
      Expect.equal (pattern (point 0. 0. 2.)) white ""

    testCase "A stripe pattern alternates in x" <| fun _ ->
      let pattern = patternAt white black Stripes
      Expect.equal (pattern (point 0. 0. 0.)) white "One"
      Expect.equal (pattern (point 0.9 0. 0.)) white "Two"
      Expect.equal (pattern (point 1. 0. 0.)) black "Three"
      Expect.equal (pattern (point -0.1 0. 0.)) black "Four"
      Expect.equal (pattern (point -1. 0. 0.)) black "Five"
      Expect.equal (pattern (point -1.1 0. 0.)) white "Six"

    testCase "Stripes with an object transformation" <| fun _ ->
      let t = uniformScale 2.f
      let object = sphereT t
      let p = point 1.5 0. 0.
      let patternP = patternPoint object.transform (identity ()) p
      let c = patternAt white black Stripes patternP
      Expect.equal c white ""

    testCase "Stripes with a pattern transformation" <| fun _ ->
      let object = unitSphere ()
      let p = point 1.5 0. 0.
      let patternP = patternPoint object.transform (uniformScale 2.f) p
      let c = patternAt white black Stripes patternP
      Expect.equal c white ""

    testCase "Stripes with both an object and a pattern transformation" <| fun _ ->
      let t = uniformScale 2.f
      let object = sphereT t
      let p = point 2.5 0. 0.
      let patternP = patternPoint object.transform (translateX 0.5f) p
      let c = patternAt white black Stripes patternP
      Expect.equal c white ""

    testCase "A gradient linearly interpolates between colors" <| fun _ ->
      let pattern = gradientAt white black 0.
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0.25 0. 0.)) (color 0.75 0.75 0.75) ""
      Expect.equal (pattern (point 0.5 0. 0.)) (color 0.5 0.5 0.5) ""
      Expect.equal (pattern (point 0.75 0. 0.)) (color 0.25 0.25 0.25) ""

    testCase "A ring should extend in both x and z" <| fun _ ->
      let pattern = patternAt white black Ring
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 1. 0. 0.)) black ""
      Expect.equal (pattern (point 0. 0. 1.)) black ""
      // 0.708 = just slightly more than √2/2​
      Expect.equal (pattern (point 0.708 0. 0.708)) black ""

    testCase "Checkers should repeat in x" <| fun _ ->
      let pattern = patternAt white black Checkers
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0.99 0. 0.)) white ""
      Expect.equal (pattern (point 1.01 0. 0.)) black ""

    testCase "Checkers should repeat in y" <| fun _ ->
      let pattern = patternAt white black Checkers
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0. 0.99 0.)) white ""
      Expect.equal (pattern (point 0. 1.01 0.)) black ""

    testCase "Checkers should repeat in z" <| fun _ ->
      let pattern = patternAt white black Checkers
      Expect.equal (pattern (point 0. 0. 0.)) white ""
      Expect.equal (pattern (point 0. 0. 0.99)) white ""
      Expect.equal (pattern (point 0. 0. 1.01)) black ""
  ]
