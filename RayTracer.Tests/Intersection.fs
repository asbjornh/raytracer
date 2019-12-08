module IntersectionTest

open Expecto
open Sphere
open Intersection

[<Tests>]
let tests =
  testList "Tests for Intersection" [
    testCase "An intersection encapsulates t and object" <| fun _ ->
      let s = sphere ()
      let i = intersection 3.5 s
      Expect.equal i.t 3.5 ""
      Expect.equal i.object s ""
  ]