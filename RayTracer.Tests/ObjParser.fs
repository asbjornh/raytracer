module ObjParserTest

open Expecto

open ObjParser
open Tuple

[<Tests>]
let tests =
  testList "Tests for ObjParser" [
    testCase "Ignoring unrecognized lines" <| fun _ ->
      let gibberish = [
        "There was a young lady named Bright​"
        "who traveled much faster than light.​"
        "She set out one day​"
        "in a relative way,​"
        "and came back the previous night.​"
      ]
      let r = parse gibberish
      Expect.equal r.ignoredLines 5 ""

    testCase "Vertex records" <| fun _ ->
      let file = [
        "v -1 1 0​"
        "v -1.0000 0.5000 0.0000​"
        "v 1 0 0​"
        "v 1 1 0​"
      ]
      let r = parse file
      Expect.equal r.vertices.[0] (point -1. 1. 0.) ""
      Expect.equal r.vertices.[1] (point -1. 0.5 0.) ""
      Expect.equal r.vertices.[2] (point 1. 0. 0.) ""
      Expect.equal r.vertices.[3] (point 1. 1. 0.) ""
  ]
