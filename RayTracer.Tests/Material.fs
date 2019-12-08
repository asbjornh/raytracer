module MaterialTest

open Expecto

open Color
open Material

[<Tests>]

let tests =
  testList "Tests for Material" [
    testCase "The default material" <| fun _ ->
      let m = material ()
      Expect.equal m.color (color 1. 1. 1.) ""
      Expect.equal m.ambient 0.1 ""
      Expect.equal m.diffuse 0.9 ""
      Expect.equal m.specular 0.9 ""
      Expect.equal m.shininess 200.0 ""
  ]
