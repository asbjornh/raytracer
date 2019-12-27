module ShapeTest

open System
open Expecto

open Tuple
open Shape
open Transform

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

let testShape t = {
  transform = t
  material = Material.defaultMaterial ()
  shape = TestShape
  parent = None
  children = []
}

[<Tests>]
let tests =
  testList "Tests for Shape" [
    testCase "Computing the normal on a translated shape" <| fun _ ->
      let s = testShape (translate 0. 1. 0.)
      let n = normalAt s (point 0. 1.70711 -0.70711)
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed shape" <| fun _ ->
      let s =
        testShape
        <| chain [scale 1. 0.5 1.; rotateZ (Math.PI / 5.)]
      let a = (sqrt 2.) / 2.
      let n = normalAt s (point 0. a -a)
      Expect.equal n (vector 0. 0.97014 -0.24254) ""
  ]
