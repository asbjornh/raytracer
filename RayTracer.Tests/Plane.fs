module PlaneTest

open Expecto

open Tuple
open Shape
open Ray

[<Tests>]
let tests =
  testList "Tests for Plane" [
    testCase "The normal of a plane is constant everywhere" <| fun _ ->
      let p = defaultPlane ()
      let n1 = localNormal p (point 0. 0. 0.)
      let n2 = localNormal p (point 10. 0. -10.)
      let n3 = localNormal p (point -5. 0. 150.)
      Expect.equal n1 (vector 0. 1. 0.) ""
      Expect.equal n2 (vector 0. 1. 0.) ""
      Expect.equal n3 (vector 0. 1. 0.) ""

    testCase "Intersect with a ray parallel to the plane" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 10. 0.) (vector 0. 0. 1.)
      let xs = localIntersect r p
      Expect.isEmpty xs ""

    testCase "Intersect with a coplanar ray" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let xs = localIntersect r p
      Expect.isEmpty xs ""

    testCase "A ray intersecting a plane from above" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 1. 0.) (vector 0. -1. 0.)
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      let (t, object) = xs.[0]
      Expect.equal t 1.f ""
      Expect.equal object p ""

    testCase "A ray intersecting a plane from below" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. -1. 0.) (vector 0. 1. 0.)
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      let (t, object) = xs.[0]
      Expect.equal t 1.f ""
      Expect.equal object p ""
  ]
