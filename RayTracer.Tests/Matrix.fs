module MatrixTest

open Expecto
open Matrix

let diff actual expected = Expect.defaultDiffPrinter expected actual

[<Tests>]

let tests =
  testList "Tests for Matrix" [
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
      Expect.isTrue (a = b) ""

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
      Expect.isFalse (a = b) ""

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
      Expect.equal (a * b) expected ""

    testCase "Identity matrix" <| fun _ ->
      let expected = matrix [
        [ 1. ; 0. ; 0. ; 0. ]
        [ 0. ; 1. ; 0. ; 0. ]
        [ 0. ; 0. ; 1. ; 0. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      Expect.equal (identity ()) expected ""

    testCase "Multiplying a matrix by the identity matrix" <| fun _ ->
      let m = matrix [
        [ 0. ; 1. ;  2. ;  4. ]
        [ 1. ; 2. ;  4. ;  8. ]
        [ 2. ; 4. ;  8. ; 16. ]
        [ 4. ; 8. ; 16. ; 32. ]
      ]
      Expect.equal (m * (identity ())) m ""

    testCase "Multiplying the identity matrix by a tuple" <| fun _ ->
      let t = Tuple.Tuple (1.f, 2.f, 3.f, 4.f)
      Expect.equal (multiplyT (identity ()) t) t ""

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
      Expect.equal (transpose (identity ())) (identity ()) ""

    testCase "Calculating the determinant of a 4x4 matrix" <| fun _ ->
      let m = matrix [
        [ -2. ; -8. ;  3. ;  5. ]
        [ -3. ;  1. ;  7. ;  3. ]
        [  1. ;  2. ; -9. ;  6. ]
        [ -6. ;  7. ;  7. ; -9. ]
      ]
      Expect.equal (determinant m) -4071.f ""

    testCase "Testing an invertible matrix for invertibility" <| fun _ ->
      let m = matrix [
        [  6. ;  4. ;  4. ;  4. ]
        [  5. ;  5. ;  7. ;  6. ]
        [  4. ; -9. ;  3. ; -7. ]
        [  9. ;  1. ;  7. ; -6. ]
      ]
      Expect.equal (determinant m) -2120.f ""
      Expect.isTrue (invertible m) ""

    testCase "Testing a noninvertible matrix for invertibility" <| fun _ ->
      let m = matrix [
        [ -4. ;  2. ; -2. ; -3. ]
        [  9. ;  6. ;  2. ;  6. ]
        [  0. ; -5. ;  1. ; -5. ]
        [  0. ;  0. ;  0. ;  0. ]
      ]
      Expect.equal (determinant m) 0.f ""
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
      Expect.equal (determinant a) 532.f ""
      Expect.isTrue (equals b expected) (diff b expected)

    testCase "Calculating the inverse of another matrix" <| fun _ ->
      let a = matrix [
        [  8. ; -5. ;  9. ;  2. ]
        [  7. ;  5. ;  6. ;  1. ]
        [ -6. ;  0. ;  9. ;  6. ]
        [ -3. ;  0. ; -9. ; -4. ]
      ]
      let b = inverse a
      let expected = matrix [
        [ -0.15385 ; -0.15385 ; -0.28205 ; -0.53846 ]
        [ -0.07692 ;  0.12308 ;  0.02564 ;  0.03077 ]
        [  0.35897 ;  0.35897 ;  0.43590 ;  0.92308 ]
        [ -0.69231 ; -0.69231 ; -0.76923 ; -1.92308 ]
      ]
      Expect.isTrue (equals b expected) (diff b expected)

    testCase "Calculating the inverse of a third matrix" <| fun _ ->
      let a = matrix [
        [  9. ;  3. ;  0. ;  9. ]
        [ -5. ; -2. ; -6. ; -3. ]
        [ -4. ;  9. ;  6. ;  4. ]
        [ -7. ;  6. ;  6. ;  2. ]
      ]
      let b = inverse a
      let expected = matrix [
        [ -0.04074 ; -0.07778 ;  0.14444 ; -0.22222 ]
        [ -0.07778 ;  0.03333 ;  0.36667 ; -0.33333 ]
        [ -0.02901 ; -0.14630 ; -0.10926 ;  0.12963 ]
        [  0.17778 ;  0.06667 ; -0.26667 ;  0.33333 ]
      ]
      Expect.isTrue (equals b expected) (diff b expected)

    testCase "Multiplying a product by its inverse" <| fun _ ->
      let a = matrix [
        [  3. ; -9. ;  7. ;  3. ]
        [  3. ; -8. ;  2. ; -9. ]
        [ -4. ;  4. ;  4. ;  1. ]
        [ -6. ;  5. ; -1. ;  1. ]
      ]
      let b = matrix [
        [  8. ;  2. ;  2. ;  2. ]
        [  3. ; -1. ;  7. ;  0. ]
        [  7. ;  0. ;  5. ;  4. ]
        [  6. ; -2. ;  0. ;  5. ]
      ]
      let c = a * b
      let reversed = c * (inverse b)
      Expect.isTrue (equals reversed a) (diff reversed a)
  ]
