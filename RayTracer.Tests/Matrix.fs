module MatrixTest

open System
open Expecto
open Matrix

let diff expected actual = Expect.defaultDiffPrinter expected actual

[<Tests>]

let tests =
  testList "Tests for Canvas" [
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

    testCase "Multiplying two matrices" <| fun _ ->
      let a = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = matrix [
        [ -2. ; 1. ; 2. ;  3. ]
        [  3. ; 2. ; 1. ; -1. ]
        [  4. ; 3. ; 6. ;  5. ]
        [  1. ; 2. ; 7. ;  8. ]
      ]
      let expected = matrix [
        [ 20. ;  22. ;  50. ;  48. ]
        [ 44. ;  54. ; 114. ; 108. ]
        [ 40. ;  58. ; 110. ; 102. ]
        [ 16. ;  26. ;  46. ;  42. ]
      ]
      Expect.equal (multiply a b) expected ""

    testCase "Get column from matrix" <| fun _ ->
      let m = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 2. ; 4. ; 4. ; 2. ]
        [ 8. ; 6. ; 4. ; 1. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      let col = [| 2.; 4.; 6.; 0. |]
      Expect.equal (getColumn 1 m) col ""

    testCase "A matrix multiplied by a tuple" <| fun _ ->
      let m = matrix [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 2. ; 4. ; 4. ; 2. ]
        [ 8. ; 6. ; 4. ; 1. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      let t = Tuple.point 1. 2. 3.
      let expected = Tuple.point 18. 24. 33.
      Expect.equal (multiplyTuple m t) expected ""

    testCase "Identity matrix" <| fun _ ->
      let expected = matrix [
        [ 1. ; 0. ; 0. ; 0. ]
        [ 0. ; 1. ; 0. ; 0. ]
        [ 0. ; 0. ; 1. ; 0. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      Expect.equal identity expected ""

    testCase "Multiplying a matrix by the identity matrix" <| fun _ ->
      let m = matrix [
        [ 0. ; 1. ;  2. ;  4. ]
        [ 1. ; 2. ;  4. ;  8. ]
        [ 2. ; 4. ;  8. ; 16. ]
        [ 4. ; 8. ; 16. ; 32. ]
      ]
      Expect.equal (multiply m identity) m ""

    testCase "Multiplying the identity matrix by a tuple" <| fun _ ->
      let t = (1., 2., 3., 4.)
      Expect.equal (multiplyTuple identity t) t ""

    testCase "Transposing a matrix" <| fun _ ->
      let m = matrix [
        [ 0. ; 9. ; 3. ; 0. ]
        [ 9. ; 8. ; 0. ; 8. ]
        [ 1. ; 8. ; 5. ; 3. ]
        [ 0. ; 0. ; 5. ; 8. ]
      ]
      let expected = matrix [
        [ 0. ; 9. ; 1. ; 0. ]
        [ 9. ; 8. ; 8. ; 0. ]
        [ 3. ; 0. ; 5. ; 5. ]
        [ 0. ; 8. ; 3. ; 8. ]
      ]
      Expect.equal (transpose m) expected ""

    testCase "Transposing the identity" <| fun _ ->
      Expect.equal (transpose identity) identity ""

    testCase "Calculating the determinant of a 2x2 matrix" <| fun _ ->
      let m = matrix [
        [  1. ; 5. ]
        [ -3. ; 2. ]
      ]
      Expect.equal (determinant m) 17. ""

    testCase "A submatrix of a 3x3 matrix is a 2x2 matrix" <| fun _ ->
      let m = matrix [
        [  1. ; 5. ;  0. ]
        [ -3. ; 2. ;  7. ]
        [  0. ; 6. ; -3. ]
      ]
      let expected = matrix [
        [ -3. ; 2. ]
        [  0. ; 6. ]
      ]
      Expect.equal (submatrix 0 2 m) expected ""

    testCase "A submatrix of a 4x4 matrix is a 3x3 matrix" <| fun _ ->
      let m = matrix [
        [ -6. ;  1. ;  1. ;  6. ]
        [ -8. ;  5. ;  8. ;  6. ]
        [ -1. ;  0. ;  8. ;  2. ]
        [ -7. ;  1. ; -1. ;  1. ]
      ]
      let expected = matrix [
        [ -6. ;  1. ; 6. ]
        [ -8. ;  8. ; 6. ]
        [ -7. ; -1. ; 1. ]
      ]
      Expect.equal (submatrix 2 1 m) expected ""

    testCase "Calculating a minor of a 3x3 matrix" <| fun _ ->
      let m = matrix [
        [  3. ;  5. ;  0. ]
        [  2. ; -1. ; -7. ]
        [  6. ; -1. ;  5. ]
      ]
      let b = submatrix 1 0 m
      Expect.equal (determinant b) 25. ""
      Expect.equal (minor 1 0 m) 25. ""

    testCase "Calculating a cofactor of a 3x3 matrix" <| fun _ ->
      let m = matrix [
        [  3. ;  5. ;  0. ]
        [  2. ; -1. ; -7. ]
        [  6. ; -1. ;  5. ]
      ]
      Expect.equal (minor 0 0 m) -12. ""
      Expect.equal (cofactor 0 0 m) -12. ""
      Expect.equal (minor 1 0 m) 25. ""
      Expect.equal (cofactor 1 0 m) -25. ""

    testCase "Calculating the determinant of a 3x3 matrix" <| fun _ ->
      let m = matrix [
        [  1. ;  2. ;  6. ]
        [ -5. ;  8. ; -4. ]
        [  2. ;  6. ;  4. ]
      ]
      Expect.equal (cofactor 0 0 m) 56. ""
      Expect.equal (cofactor 0 1 m) 12. ""
      Expect.equal (cofactor 0 2 m) -46. ""
      Expect.equal (determinant m) -196. ""

    testCase "Calculating the determinant of a 4x4 matrix" <| fun _ ->
      let m = matrix [
        [ -2. ; -8. ;  3. ;  5. ]
        [ -3. ;  1. ;  7. ;  3. ]
        [  1. ;  2. ; -9. ;  6. ]
        [ -6. ;  7. ;  7. ; -9. ]
      ]
      Expect.equal (cofactor 0 0 m) 690. ""
      Expect.equal (cofactor 0 1 m) 447. ""
      Expect.equal (cofactor 0 2 m) 210. ""
      Expect.equal (cofactor 0 3 m) 51. ""
      Expect.equal (determinant m) -4071. ""

    testCase "Testing an invertible matrix for invertibility" <| fun _ ->
      let m = matrix [
        [  6. ;  4. ;  4. ;  4. ]
        [  5. ;  5. ;  7. ;  6. ]
        [  4. ; -9. ;  3. ; -7. ]
        [  9. ;  1. ;  7. ; -6. ]
      ]
      Expect.equal (determinant m) -2120. ""
      Expect.isTrue (invertible m) ""

    testCase "Testing a noninvertible matrix for invertibility" <| fun _ ->
      let m = matrix [
        [ -4. ;  2. ; -2. ; -3. ]
        [  9. ;  6. ;  2. ;  6. ]
        [  0. ; -5. ;  1. ; -5. ]
        [  0. ;  0. ;  0. ;  0. ]
      ]
      Expect.equal (determinant m) 0. ""
      Expect.isFalse (invertible m) ""

    testCase "Calculating the inverse of a matrix" <| fun _ ->
      let a = matrix [
        [ -5. ;  2. ;  6. ; -8. ]
        [  1. ; -5. ;  1. ;  8. ]
        [  7. ;  7. ; -6. ; -7. ]
        [  1. ; -3. ;  7. ;  4. ]
      ]
      let b = inverse a
      let expected = matrix [
        [  0.21805 ;  0.45113 ;  0.24060 ; -0.04511 ]
        [ -0.80827 ; -1.45677 ; -0.44361 ;  0.52068 ]
        [ -0.07895 ; -0.22368 ; -0.05263 ;  0.19737 ]
        [ -0.52256 ; -0.81391 ; -0.30075 ;  0.30639 ]
      ]
      Expect.equal (determinant a) 532. ""
      Expect.equal (cofactor 2 3 a) -160. ""
      Expect.equal (b.[3].[2]) (-160. / 532.) ""
      Expect.equal (cofactor 3 2 a) 105. ""
      Expect.equal (b.[2].[3]) (105. / 532.) ""
      Expect.isTrue (equals b expected) (diff expected b)
  ]
