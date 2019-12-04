module CanvasTest

open Expecto
open Canvas
open Color

let black = color 0.0 0.0 0.0
let red = color 1.0 0.0 0.0
let collapse l = List.reduce (List.append) l
let isBlack = equals black

[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Creating a canvas" <| fun _ ->
      let c = canvas 10 20
      Expect.equal (width c) 10 ""
      Expect.equal (height c) 20 ""
      Expect.isTrue (collapse c |> List.forall isBlack) ""

    testCase "Writing pixels to a canvas" <| fun _ ->
      let c1 = canvas 10 20
      let c2 = write c1 2 3 red
      Expect.equal (read c2 2 3) (Some red) ""
  ]
