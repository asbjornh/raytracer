module TransformTest

open Expecto
open Matrix
open Transform
open Tuple

[<Tests>]
let tests =
  testList "Tests for Transform" [
    testCase "Multiplying by a translation matrix" <| fun _ ->
      let transform = translation 5. -3. 2.
      let p = point -3. 4. 5.
      Expect.equal (multiplyT transform p) (point 2. 1. 7.) ""

    testCase "Multiplying by the inverse of a translation matrix" <| fun _ ->
      let transform = translation 5. -3. 2.
      let p = point -3. 4. 5.
      Expect.equal (multiplyT (inverse transform) p) (point -8. 7. 3.) ""

    testCase "Translation does not affect vectors" <| fun _ ->
      let transform = translation 5. -3. 2.
      let v = vector -3. 4. 5.
      Expect.equal (multiplyT transform v) v ""

    testCase "A scaling matrix applied to a point" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let p = point -4. 6. 8.
      Expect.equal (multiplyT transform p) (point -8. 18. 32.) ""

    testCase "A scaling matrix applied to a vector" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let v = vector -4. 6. 8.
      Expect.equal (multiplyT transform v) (vector -8. 18. 32.) ""

    testCase "Multiplying by the inverse of a scaling matrix" <| fun _ ->
      let transform = scaling 2. 3. 4.
      let v = vector -4. 6. 8.
      Expect.equal (multiplyT (inverse transform) v) (vector -2. 2. 2.) ""

    testCase "Reflection is scaling by a negative value" <| fun _ ->
      let transform = scaling -1. 1. 1.
      let p = point 2. 3. 4.
      Expect.equal (multiplyT transform p) (point -2. 3. 4.) ""

    testCase "Rotating a point around the x axis" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotationX (Util.rad 45.)
      let fullQuarter = rotationX (Util.rad 90.)
      let a = (sqrt 2.) / 2.

      let result1 = multiplyT halfQuarter p
      let expected1 = point 0. a a
      Expect.equal result1 expected1 ""

      let result2 = multiplyT fullQuarter p
      let expected2 = point 0. 0. 1.
      Expect.equal result2 expected2 ""

    testCase "The inverse of an x-rotation rotates in the opposite direction" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotationX (Util.rad 45.)
      let inv = inverse halfQuarter

      let a = (sqrt 2.) / 2.
      let result = multiplyT inv p
      let expected = point 0. a -a
      Expect.equal result expected ""

    testCase "Rotating a point around the y axis" <| fun _ ->
      let p = point 0. 0. 1.
      let halfQuarter = rotationY (Util.rad 45.)
      let fullQuarter = rotationY (Util.rad 90.)

      let a = (sqrt 2.) / 2.
      let result1 = multiplyT halfQuarter p
      let expected1 = point a 0. a
      Expect.equal result1 expected1 ""

      let result2 = multiplyT fullQuarter p
      let expected2 = point 1. 0. 0.
      Expect.equal result2 expected2 ""

    testCase "Rotating a point around the z axis" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotationZ (Util.rad 45.)
      let fullQuarter = rotationZ (Util.rad 90.)

      let a = (sqrt 2.) / 2.
      let result1 = multiplyT halfQuarter p
      let expected1 = point -a a 0.
      Expect.equal result1 expected1 ""
      
      let result2 = multiplyT fullQuarter p
      let expected2 = point -1. 0. 0.
      Expect.equal result2 expected2 ""

    testCase "A shearing transformation moves x in proportion to y" <| fun _ ->
      let transform = shearing 1. 0. 0. 0. 0. 0.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 5. 3. 4.
      Expect.equal result expected ""

    testCase "A shearing transformation moves x in proportion to z" <| fun _ ->
      let transform = shearing 0. 1. 0. 0. 0. 0.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 6. 3. 4.
      Expect.equal result expected ""

    testCase "A shearing transformation moves y in proportion to x" <| fun _ ->
      let transform = shearing 0. 0. 1. 0. 0. 0.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 2. 5. 4.
      Expect.equal result expected ""

    testCase "A shearing transformation moves y in proportion to z" <| fun _ ->
      let transform = shearing 0. 0. 0. 1. 0. 0.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 2. 7. 4.
      Expect.equal result expected ""

    testCase "A shearing transformation moves z in proportion to x" <| fun _ ->
      let transform = shearing 0. 0. 0. 0. 1. 0.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 2. 3. 6.
      Expect.equal result expected ""

    testCase "A shearing transformation moves z in proportion to y" <| fun _ ->
      let transform = shearing 0. 0. 0. 0. 0. 1.
      let p = point 2. 3. 4.
      let result = multiplyT transform p
      let expected = point 2. 3. 7.
      Expect.equal result expected ""

    testCase "Individual transformations are applied in sequence" <| fun _ ->
      let p = point 1. 0. 1.
      let a = rotationX (Util.rad 90.)
      let b = scaling 5. 5. 5.
      let c = translation 10. 5. 7.
      // apply rotation first​
      let p2 = multiplyT a p
      Expect.equal p2 (point 1. -1. 0.) ""

      // then apply scaling​
      let p3 = multiplyT b p2
      Expect.equal p3 (point 5. -5. 0.) ""

      // then apply translation​
      let p4 = multiplyT c p3
      Expect.equal p4 (point 15. 0. 7.) ""

    testCase "Chained transformations must be applied in reverse order" <| fun _ ->
      let p = point 1. 0. 1.
      let a = rotationX (Util.rad 90.)
      let b = scaling 5. 5. 5.
      let c = translation 10. 5. 7.
      let t = c * b * a
      Expect.equal (multiplyT t p) (point 15. 0. 7.) ""

    testCase "The chain function applies transforms in sequence" <| fun _ ->
      let p = point 0. 0. 0.
      let t = chain [
        scale 5. 5. 5.
        translate 10. 10. 10.
      ]
      Expect.equal (multiplyT t p) (point 50. 50. 50.) ""

    testCase "The transformation matrix for the default orientation" <| fun _ ->
      let from = point 0. 0. 0.
      let To = point 0. 0. -1.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t (identity ()) ""

    testCase "A view transformation matrix looking in positive z direction" <| fun _ ->
      let from = point 0. 0. 0.
      let To = point 0. 0. 1.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t (scaling -1. 1. -1.) ""

    testCase "The view transformation moves the world" <| fun _ ->
      let from = point 0. 0. 8.
      let To = point 0. 0. 0.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t (translation 0. 0. -8.) ""

    testCase "An arbitrary view transformation" <| fun _ ->
      let from = point 1. 3. 2.
      let To = point 4. -2. 8.
      let up = vector 1. 1. 0.
      let t = viewTransform from To up
      let expected = matrix [
        [ -0.50709 ; 0.50709 ;  0.67612 ; -2.36643 ]
        [  0.76772 ; 0.60609 ;  0.12122 ; -2.82843 ]
        [ -0.35857 ; 0.59761 ; -0.71714 ;  0.00000 ]
        [  0.00000 ; 0.00000 ;  0.00000 ;  1.00000 ]
      ]
      Expect.equal t expected ""
  ]
