module TransformTest

open Expecto
open Matrix
open Transform

let diff actual expected = Expect.defaultDiffPrinter expected actual

let expectTupleEquals expected actual =
  Expect.isTrue (Tuple.equals actual expected) (diff actual expected)

[<Tests>]
let tests =
  testList "Tests for Transform" [
    testCase "Multiplying by a translation matrix" <| fun _ ->
      let transform = translation 5. -3. 2.
      let p = Tuple.point -3. 4. 5.
      Expect.equal (multiplyTuple transform p) (Tuple.point 2. 1. 7.) ""

    testCase "Multiplying by the inverse of a translation matrix" <| fun _ ->
      let transform = translation 5. -3. 2.
      let p = Tuple.point -3. 4. 5.
      Expect.equal (multiplyTuple (inverse transform) p) (Tuple.point -8. 7. 3.) ""

    testCase "Translation does not affect vectors" <| fun _ ->
      let transform = translation 5. -3. 2.
      let v = Tuple.vector -3. 4. 5.
      Expect.equal (multiplyTuple transform v) v ""

    testCase "A scaling matrix applied to a point" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let p = Tuple.point -4. 6. 8.
      Expect.equal (multiplyTuple transform p) (Tuple.point -8. 18. 32.) ""

    testCase "A scaling matrix applied to a vector" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let v = Tuple.vector -4. 6. 8.
      Expect.equal (multiplyTuple transform v) (Tuple.vector -8. 18. 32.) ""

    testCase "Multiplying by the inverse of a scaling matrix" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let v = Tuple.vector -4. 6. 8.
      Expect.equal (multiplyTuple (inverse transform) v) (Tuple.vector -2. 2. 2.) ""

    testCase "Reflection is scaling by a negative value" <| fun _ ->
      let transform = scaling -1. 1. 1.
      let p = Tuple.point 2. 3. 4.
      Expect.equal (multiplyTuple transform p) (Tuple.point -2. 3. 4.) ""

    testCase "Rotating a point around the x axis" <| fun _ ->
      let p = Tuple.point 0. 1. 0.
      let halfQuarter = rotationX (Util.rad 45.)
      let fullQuarter = rotationX (Util.rad 90.)
      let a = (sqrt 2.) / 2.

      let result1 = multiplyTuple halfQuarter p
      let expected1 = Tuple.point 0. a a
      expectTupleEquals result1 expected1

      let result2 = multiplyTuple fullQuarter p
      let expected2 = Tuple.point 0. 0. 1.
      expectTupleEquals result2 expected2

    testCase "The inverse of an x-rotation rotates in the opposite direction" <| fun _ ->
      let p = Tuple.point 0. 1. 0.
      let halfQuarter = rotationX (Util.rad 45.)
      let inv = inverse halfQuarter

      let a = (sqrt 2.) / 2.
      let result = multiplyTuple inv p
      let expected = Tuple.point 0. a -a
      expectTupleEquals result expected

    testCase "Rotating a point around the y axis" <| fun _ ->
      let p = Tuple.point 0. 0. 1.
      let halfQuarter = rotationY (Util.rad 45.)
      let fullQuarter = rotationY (Util.rad 90.)

      let a = (sqrt 2.) / 2.
      let result1 = multiplyTuple halfQuarter p
      let expected1 = Tuple.point a 0. a
      expectTupleEquals result1 expected1

      let result2 = multiplyTuple fullQuarter p
      let expected2 = Tuple.point 1. 0. 0.
      expectTupleEquals result2 expected2

    testCase "Rotating a point around the z axis" <| fun _ ->
      let p = Tuple.point 0. 1. 0.
      let halfQuarter = rotationZ (Util.rad 45.)
      let fullQuarter = rotationZ (Util.rad 90.)

      let a = (sqrt 2.) / 2.
      let result1 = multiplyTuple halfQuarter p
      let expected1 = Tuple.point -a a 0.
      expectTupleEquals result1 expected1
      
      let result2 = multiplyTuple fullQuarter p
      let expected2 = Tuple.point -1. 0. 0.
      expectTupleEquals result2 expected2

    testCase "A shearing transformation moves x in proportion to y" <| fun _ ->
      let transform = shearing 1. 0. 0. 0. 0. 0.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 5. 3. 4.
      expectTupleEquals result expected

    testCase "A shearing transformation moves x in proportion to z" <| fun _ ->
      let transform = shearing 0. 1. 0. 0. 0. 0.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 6. 3. 4.
      expectTupleEquals result expected

    testCase "A shearing transformation moves y in proportion to x" <| fun _ ->
      let transform = shearing 0. 0. 1. 0. 0. 0.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 2. 5. 4.
      expectTupleEquals result expected

    testCase "A shearing transformation moves y in proportion to z" <| fun _ ->
      let transform = shearing 0. 0. 0. 1. 0. 0.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 2. 7. 4.
      expectTupleEquals result expected

    testCase "A shearing transformation moves z in proportion to x" <| fun _ ->
      let transform = shearing 0. 0. 0. 0. 1. 0.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 2. 3. 6.
      expectTupleEquals result expected

    testCase "A shearing transformation moves z in proportion to y" <| fun _ ->
      let transform = shearing 0. 0. 0. 0. 0. 1.
      let p = Tuple.point 2. 3. 4.
      let result = multiplyTuple transform p
      let expected = Tuple.point 2. 3. 7.
      expectTupleEquals result expected

    testCase "Individual transformations are applied in sequence" <| fun _ ->
      let p = Tuple.point 1. 0. 1.
      let a = rotationX (Util.rad 90.)
      let b = scaling 5. 5. 5.
      let c = translation 10. 5. 7.
      // apply rotation first​
      let p2 = multiplyTuple a p
      expectTupleEquals p2 (Tuple.point 1. -1. 0.)

      // then apply scaling​
      let p3 = multiplyTuple b p2
      expectTupleEquals p3 (Tuple.point 5. -5. 0.)

      // then apply translation​
      let p4 = multiplyTuple c p3
      expectTupleEquals p4 (Tuple.point 15. 0. 7.)

    testCase "Chained transformations must be applied in reverse order" <| fun _ ->
      let p = Tuple.point 1. 0. 1.
      let a = rotationX (Util.rad 90.)
      let b = scaling 5. 5. 5.
      let c = translation 10. 5. 7.
      let t = multiply c (multiply b a)
      expectTupleEquals (multiplyTuple t p) (Tuple.point 15. 0. 7.)

    testCase "The chain function applies transforms in sequence" <| fun _ ->
      let p = Tuple.point 1. 0. 1.
      let t = chain [
        rotateX (Util.rad 90.)
        scale 5. 5. 5.
        translate 10. 5. 7.
      ]
      expectTupleEquals (multiplyTuple t p) (Tuple.point 15. 0. 7.)
  ]