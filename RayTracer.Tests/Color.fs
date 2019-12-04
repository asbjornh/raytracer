module ColorTest

open Expecto
open Color

[<Tests>]

let tests =
  testList "Tests for Unit" [
    testCase "Colors are (red, green, blue) tuples" <| fun _ ->
      let c = color -0.5 0.4 1.7
      Expect.equal (c) (-0.5, 0.4, 1.7) ""

    testCase "Adding colors" <| fun _ ->
      let c1 = color 0.9 0.6 0.75
      let c2 = color 0.7 0.1 0.25
      Expect.equal (add c1 c2) (color 1.6 0.7 1.0) ""

    testCase "Subtracting colors" <| fun _ ->
      let c1 = color 0.9 0.6 0.75
      let c2 = color 0.7 0.1 0.25
      let c3 = subtract c1 c2
      Expect.isTrue (equals c3 (color 0.2 0.5 0.5)) ""

    testCase "Multiplying a color by a scalar" <| fun _ ->
      let c = color 0.2 0.3 0.4
      Expect.equal (scale 2.0 c) (color 0.4 0.6 0.8) ""

    testCase "Multiplying colors" <| fun _ ->
      let c1 = color 1.0 0.2 0.4
      let c2 = color 0.9 1.0 0.1
      let c3 = multiply c1 c2
      Expect.isTrue (equals c3 (color 0.9 0.2 0.04)) ""
  ]