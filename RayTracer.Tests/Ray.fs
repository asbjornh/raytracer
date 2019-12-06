module RayTest

open Expecto
open Tuple
open Ray

[<Tests>]
let tests =
  testList "Tests for Ray" [
    testCase "Creating and querying a ray" <| fun _ ->
      let origin = point 1. 2. 3.
      let direction = vector 4. 5. 6.
      let r = ray origin direction
      Expect.equal (r.origin) origin ""
      Expect.equal (r.direction) direction ""

    testCase "Computing a point from a distance" <| fun _ ->
      let r = ray (point 2. 3. 4.) (vector 1. 0. 0.)
      Expect.equal (position 0. r) (point 2. 3. 4.) ""
      Expect.equal (position 1. r) (point 3. 3. 4.) ""
      Expect.equal (position -1. r) (point 1. 3. 4.) ""
      Expect.equal (position 2.5 r) (point 4.5 3. 4.) ""
  ]