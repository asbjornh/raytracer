module CanvasTest

open Expecto
open Canvas

[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Creating a canvas" <| fun _ ->
     let c = canvas 10 20
     Expect.equal (width c) 10 ""
     Expect.equal (height c) 20 ""
      // ​And​ every pixel of c is color(0, 0, 0)”
  ]