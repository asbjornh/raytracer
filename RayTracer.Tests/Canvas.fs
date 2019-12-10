module CanvasTest

open Expecto
open Canvas
open Color

let black = color 0.0 0.0 0.0
let red = color 1.0 0.0 0.0
let collapse l = Array.reduce (Array.append) l
let isBlack = equals black

[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Creating a canvas" <| fun _ ->
      let c = canvas 10 20
      Expect.equal (width c) 10 ""
      Expect.equal (height c) 20 ""
      Expect.isTrue (collapse c |> Array.forall isBlack) ""

    testCase "Writing pixels to a canvas" <| fun _ ->
      let c1 = canvas 10 20
      let c2 = write 2 3 red c1
      Expect.equal (read 2 3 c2) (Some red) ""

    testCase "Constructing the PPM header" <| fun _ ->
      let ppm = toPpm (canvas 5 3)
      let expected = [|"P3"; "5 3"; "255"|]
      Expect.equal (Array.take 3 ppm) expected ""

    testCase "Constructing the PPM pixel data" <| fun _ ->
      let col1 = color 1.5 0.0 0.0
      let col2 = color 0.0 0.5 0.0
      let col3 = color -0.5 0.0 1.0
      let c1 = canvas 5 3
      let c2 = write 0 0 col1 c1
      let c3 = write 2 1 col2 c2
      let c4 = write 4 2 col3 c3
      let ppm = toPpm c4
      let expected = [|
        "P3"
        "5 3"
        "255"
        "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
        "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
        "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
      |]
      Expect.equal (ppm |> Array.take 6) expected ""

    testCase "PPM files are terminated by a newline character" <| fun _ ->
      let ppm = toPpm (canvas 5 3)
      Expect.equal (Array.last ppm) ("") ""
  ]
