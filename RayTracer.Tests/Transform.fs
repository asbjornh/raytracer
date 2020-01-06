module TransformTest

open TestUtils

open Expecto
open Matrix
open Transform
open Tuple

[<Tests>]
let tests =
  testList "Tests for Transform" [
    testCase "Multiplying by a translation matrix" <| fun _ ->
      let t = translate 5.f -3.f 2.f
      let p = point -3. 4. 5.
      expectTupleEq (transform t p) (point 2. 1. 7.)

    testCase "Multiplying by the inverse of a translation matrix" <| fun _ ->
      let t = translate 5.f -3.f 2.f
      let p = point -3. 4. 5.
      expectTupleEq (transform (inverse t) p) (point -8. 7. 3.)

    testCase "Translation does not affect vectors" <| fun _ ->
      let t = translate 5.f -3.f 2.f
      let v = vector -3. 4. 5.
      expectTupleEq (transform t v) v

    testCase "A scaling matrix applied to a point" <| fun _ ->
      let t = scale 2.f 3.f 4.f
      let p = point -4. 6. 8.
      expectTupleEq (transform t p) (point -8. 18. 32.)

    testCase "A scaling matrix applied to a vector" <| fun _ ->
      let t = scale 2.f 3.f 4.f
      let v = vector -4. 6. 8.
      expectTupleEq (transform t v) (vector -8. 18. 32.)

    testCase "Multiplying by the inverse of a scaling matrix" <| fun _ ->
      let t = scale 2.f 3.f 4.f
      let v = vector -4. 6. 8.
      expectTupleEq (transform (inverse t) v) (vector -2. 2. 2.)

    testCase "Reflection is scaling by a negative value" <| fun _ ->
      let t = scale -1.f 1.f 1.f
      let p = point 2. 3. 4.
      expectTupleEq (transform t p) (point -2. 3. 4.)

    testCase "Rotating a point around the x axis" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotateX (Util.rad32 45.f)
      let fullQuarter = rotateX (Util.rad32 90.f)
      let a = (sqrt 2.) / 2.

      let result1 = transform halfQuarter p
      let expected1 = point 0. a a
      expectTupleEq result1 expected1

      let result2 = transform fullQuarter p
      let expected2 = point 0. 0. 1.
      expectTupleEq result2 expected2

    testCase "The inverse of an x-rotation rotates in the opposite direction" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotateX (Util.rad32 45.f)
      let inv = inverse halfQuarter

      let a = (sqrt 2.) / 2.
      let result = transform inv p
      let expected = point 0. a -a
      expectTupleEq result expected

    testCase "Rotating a point around the y axis" <| fun _ ->
      let p = point 0. 0. 1.
      let halfQuarter = rotateY (Util.rad32 45.f)
      let fullQuarter = rotateY (Util.rad32 90.f)

      let a = (sqrt 2.) / 2.
      let result1 = transform halfQuarter p
      let expected1 = point a 0. a
      expectTupleEq result1 expected1

      let result2 = transform fullQuarter p
      let expected2 = point 1. 0. 0.
      expectTupleEq result2 expected2

    testCase "Rotating a point around the z axis" <| fun _ ->
      let p = point 0. 1. 0.
      let halfQuarter = rotateZ (Util.rad32 45.f)
      let fullQuarter = rotateZ (Util.rad32 90.f)

      let a = (sqrt 2.) / 2.
      let result1 = transform halfQuarter p
      let expected1 = point -a a 0.
      expectTupleEq result1 expected1
      
      let result2 = transform fullQuarter p
      let expected2 = point -1. 0. 0.
      expectTupleEq result2 expected2

    testCase "Individual transformations are applied in sequence" <| fun _ ->
      let p = point 1. 0. 1.
      let a = rotateX (Util.rad32 90.f)
      let b = uniformScale 5.f
      let c = translate 10.f 5.f 7.f
      // apply rotation first​
      let p2 = transform a p
      expectTupleEq p2 (point 1. -1. 0.)

      // then apply scaling​
      let p3 = transform b p2
      expectTupleEq p3 (point 5. -5. 0.)

      // then apply translation​
      let p4 = transform c p3
      expectTupleEq p4 (point 15. 0. 7.)

    testCase "The chain function applies transforms in sequence" <| fun _ ->
      let p = point 1. 0. 1.
      let t = chain [
        translate 10.f 5.f 7.f
        uniformScale 5.f
        rotateX (Util.rad32 90.f)
      ]
      expectTupleEq (transform t p) (point 15. 0. 7.)

    testCase "The transformation matrix for the default orientation" <| fun _ ->
      let from = point 0. 0. 0.
      let To = point 0. 0. -1.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t identity ""

    testCase "A view transformation matrix looking in positive z direction" <| fun _ ->
      let from = point 0. 0. 0.
      let To = point 0. 0. 1.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t (scale -1.f 1.f -1.f) ""

    testCase "The view transformation moves the world" <| fun _ ->
      let from = point 0. 0. 8.
      let To = point 0. 0. 0.
      let up = vector 0. 1. 0.
      let t = viewTransform from To up
      Expect.equal t (translateZ -8.f) ""
  ]
