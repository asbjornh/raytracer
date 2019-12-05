module MatrixTest

open Expecto
open Matrix

[<Tests>]

let tests =
  testList "Tests for Canvas" [
    testCase "Matrix equality with identical matrices" <| fun _ ->
      let a = array2D [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = array2D [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      Expect.isTrue (equals a b) ""

    testCase "Matrix equality with different matrices" <| fun _ ->
      let a = array2D [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = array2D [
        [ 2. ; 3. ; 4. ; 5. ]
        [ 6. ; 7. ; 8. ; 9. ]
        [ 8. ; 7. ; 6. ; 5. ]
        [ 4. ; 3. ; 2. ; 1. ]
      ]
      Expect.isFalse (equals a b) ""

    testCase "Multiplying two matrices" <| fun _ ->
      let a = array2D [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 5. ; 6. ; 7. ; 8. ]
        [ 9. ; 8. ; 7. ; 6. ]
        [ 5. ; 4. ; 3. ; 2. ]
      ]
      let b = array2D [
        [ -2. ; 1. ; 2. ;  3. ]
        [  3. ; 2. ; 1. ; -1. ]
        [  4. ; 3. ; 6. ;  5. ]
        [  1. ; 2. ; 7. ;  8. ]
      ]
      let expected = array2D [
        [ 20. ;  22. ;  50. ;  48. ]
        [ 44. ;  54. ; 114. ; 108. ]
        [ 40. ;  58. ; 110. ; 102. ]
        [ 16. ;  26. ;  46. ;  42. ]
      ]
      Expect.equal (multiply a b) expected ""

    testCase "A matrix multiplied by a tuple" <| fun _ ->
      let m = array2D [
        [ 1. ; 2. ; 3. ; 4. ]
        [ 2. ; 4. ; 4. ; 2. ]
        [ 8. ; 6. ; 4. ; 1. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      let t = Tuple.point 1. 2. 3.
      let expected = Tuple.point 18. 24. 33.
      Expect.equal (multiplyTuple m t) expected ""

    testCase "Identity matrix" <| fun _ ->
      let expected = array2D [
        [ 1. ; 0. ; 0. ; 0. ]
        [ 0. ; 1. ; 0. ; 0. ]
        [ 0. ; 0. ; 1. ; 0. ]
        [ 0. ; 0. ; 0. ; 1. ]
      ]
      Expect.equal identity expected ""

    testCase "Multiplying a matrix by the identity matrix" <| fun _ ->
      let m = array2D [
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
      let m = array2D [
        [ 0. ; 9. ; 3. ; 0. ]
        [ 9. ; 8. ; 0. ; 8. ]
        [ 1. ; 8. ; 5. ; 3. ]
        [ 0. ; 0. ; 5. ; 8. ]
      ]
      let expected = array2D [
        [ 0. ; 9. ; 1. ; 0. ]
        [ 9. ; 8. ; 8. ; 0. ]
        [ 3. ; 0. ; 5. ; 5. ]
        [ 0. ; 8. ; 3. ; 8. ]
      ]
      Expect.equal (transpose m) expected ""

    testCase "Transposing the identity" <| fun _ ->
      Expect.equal (transpose identity) identity ""

    testCase "Calculating the determinant of a 2x2 matrix" <| fun _ ->
      let m = array2D [
        [  1. ; 5. ]
        [ -3. ; 2. ]
      ]
      Expect.equal (determinant m) 17. ""
  ]
