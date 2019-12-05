module MatrixTest

open Expecto
open Matrix

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
  ]
