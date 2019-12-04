module CanvasTest

open Expecto
open Canvas
open Color

let collapse l = List.reduce (List.append) l
let isBlack = equals (color 0.0 0.0 0.0)

[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Creating a canvas" <| fun _ ->
     let c = canvas 10 20
     Expect.equal (width c) 10 ""
     Expect.equal (height c) 20 ""
     Expect.isTrue (collapse c |> List.forall isBlack) ""
  ]