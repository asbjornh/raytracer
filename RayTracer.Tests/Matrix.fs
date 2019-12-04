module MatrixTest

open Expecto
open Matrix


[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Constructing and inspecting a 4x4 matrix" <| fun _ ->
      let m = matrix [
        [ 1.0 ; 2.0 ; 3.0 ; 4.0 ]
        [ 5.5 ; 6.5 ; 7.5 ; 8.5 ]
        [ 9.0 ; 10. ; 11. ; 12. ]
        [ 13.5 ; 14.5 ; 15.5 ; 16.5 ]
      ]
      Expect.equal (m.[0].[0]) 1. ""
      Expect.equal (m.[0].[3]) 4. ""
      Expect.equal (m.[1].[0]) 5.5 ""
      Expect.equal (m.[1].[2]) 7.5 ""
      Expect.equal (m.[2].[2]) 11. ""
      Expect.equal (m.[3].[0]) 13.5 ""
      Expect.equal (m.[3].[2]) 15.5 ""

    testCase "A 2x2 matrix ought to be representable" <| fun _ ->
      let m = matrix [
        [ -3. ; 5. ]
        [ 1.0 ; -2. ]
      ]
      Expect.equal (m.[0].[0]) -3. ""
      Expect.equal (m.[1].[0]) 1. ""
      Expect.equal (m.[0].[1]) 5. ""
      Expect.equal (m.[1].[1]) -2. ""

    testCase "A 3x3 matrix ought to be representable" <| fun _ ->
      let m = matrix [
        [ -3. ;  5. ;  0. ]
        [  1. ; -2. ; -7. ]
        [  0. ;  1. ;  1. ]
      ]
      Expect.equal (m.[0].[0]) -3. ""
      Expect.equal (m.[1].[1]) -2. ""
      Expect.equal (m.[2].[2]) 1. ""

    testCase "Matrix equality with identical matrices" <| fun _ ->
      let a = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      Expect.isTrue (equals a b) ""

    testCase "Matrix equality with different matrices" <| fun _ ->
      let a = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = matrix [
        [ 2. ; 3. ; 4. ; 5. ]
        [ 6. ; 7. ; 8. ; 9. ]
        [ 8. ; 7. ; 6. ; 5. ]
        [ 4. ; 3. ; 2. ; 1. ]
      ]
      Expect.isFalse (equals a b) ""
  ]
