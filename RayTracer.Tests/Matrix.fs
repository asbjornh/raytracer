module MatrixTest

open Expecto
open Matrix


[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Constructing and inspecting a 4x4 matrix" <| fun _ ->
      let (m: Matrix) = [|
        [| 1.; 2.; 3.; 4. |]
        [| 5.5; 6.5; 7.5; 8.5 |]
        [| 9.; 10.; 11.; 12.; |]
        [| 13.5; 14.5; 15.5; 16.5; |]
      |]
      Expect.equal (m.[0].[0]) 1. ""
      Expect.equal (m.[0].[3]) 4. ""
      Expect.equal (m.[1].[0]) 5.5 ""
      Expect.equal (m.[1].[2]) 7.5 ""
      Expect.equal (m.[2].[2]) 11. ""
      Expect.equal (m.[3].[0]) 13.5 ""
      Expect.equal (m.[3].[2]) 15.5 ""
  ]