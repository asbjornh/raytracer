module LightTest

open Expecto

open TestUtils

open Color
open Light
open Tuple

[<Tests>]

let tests =
  testList "Tests for Light" [
    testCase "A point light has a position and intensity" <| fun _ ->
      let intensity = color 1. 1. 1.
      let position = point 0. 0. 0.
      let light = (pointLightFactory position intensity)
      expectTupleEq (light.position) position
      Expect.equal (light.intensity) intensity ""
  ]
