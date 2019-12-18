module UtilTest

open Expecto

open Util

let m = [|
  [| 1; 2; 3; 4 |]
  [| 5; 6; 7; 8 |]
  [| 9; 10; 11; 12 |]
  [| 13; 14; 15; 16 |]
|]

[<Tests>]
let tests =
  testList "Tests for Util" [
    testCase "Sub lower clipped" <| fun _ ->
      let a = [| 1; 2; 3 |]
      Expect.equal (sub -1 1 a) [| 1; 2 |] ""

    testCase "Sub upper clipped" <| fun _ ->
      let a = [| 1; 2; 3 |]
      Expect.equal (sub 1 3 a) [| 2; 3 |] ""

    testCase "SubGrid upper left" <| fun _ ->
      let expected = [|
        [| 1; 2; |]
        [| 5; 6; |]
      |]
      Expect.equal (subGrid 0 0 1 m) expected ""

    testCase "SubGrid upper right" <| fun _ ->
      let expected = [|
        [| 3; 4; |]
        [| 7; 8; |]
      |]
      Expect.equal (subGrid 3 0 1 m) expected ""

    testCase "SubGrid lower left" <| fun _ ->
      let expected = [|
        [| 9; 10; |]
        [| 13; 14; |]
      |]
      Expect.equal (subGrid 0 3 1 m) expected ""

    testCase "SubGrid lower right" <| fun _ ->
      let expected = [|
        [| 11; 12; |]
        [| 15; 16; |]
      |]
      Expect.equal (subGrid 3 3 1 m) expected ""

    testCase "SubGrid middle" <| fun _ ->
      let expected = [|
        [| 1; 2; 3; |]
        [| 5; 6; 7; |]
        [| 9; 10; 11; |]
      |]
      Expect.equal (subGrid 1 1 1 m) expected ""

    testCase "SubGrid upper left size 2" <| fun _ ->
      let expected = [|
        [| 1; 2; 3; |]
        [| 5; 6; 7; |]
        [| 9; 10; 11; |]
      |]
      Expect.equal (subGrid 0 0 2 m) expected ""
  ]
