module IntersectionTest

open Expecto

open Intersection
open Ray
open Shape
open World
open Tuple

[<Tests>]
let tests =
  testList "Tests for Intersection" [
    testCase "An intersection encapsulates t and object" <| fun _ ->
      let s = unitSphere ()
      let i = intersection 3.5 s
      Expect.equal i.t 3.5 ""
      Expect.equal i.object (s :> IShape) ""

    testCase "The hit, when all intersections have positive t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection 1. s
      let i2 = intersection 2. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i1) ""

    testCase "The hit, when some intersections have negative t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection -1. s
      let i2 = intersection 1. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i2) ""

    testCase "The hit, when all intersections have negative t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection -2. s
      let i2 = intersection -1. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) None ""

    testCase "The hit is always the lowest nonnegative intersection" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection 5. s
      let i2 = intersection 7. s
      let i3 = intersection -3. s
      let i4 = intersection 2. s
      let xs = intersections [i1; i2; i3; i4]
      Expect.equal (hit xs) (Some i4) ""

    testCase "Precomputing the state of an intersection" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 4. shape
      let comps = prepareComputations i r
      Expect.equal comps.t i.t ""
      Expect.equal comps.object i.object ""
      Expect.equal comps.point (point 0. 0. -1.) ""
      Expect.equal comps.eyeV (vector 0. 0. -1.) ""
      Expect.equal comps.normalV (vector 0. 0. -1.) ""

    testCase "The hit, when an intersection occurs on the outside" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 4. shape
      let comps = prepareComputations i r
      Expect.equal comps.inside false ""

    testCase "The hit, when an intersection occurs on the inside" <| fun _ ->
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 1. shape
      let comps = prepareComputations i r
      Expect.equal comps.point (point 0. 0. 1.) ""
      Expect.equal comps.eyeV (vector 0. 0. -1.) ""
      Expect.equal comps.inside true ""
      // normal would have been (0, 0, 1), but is inverted!​
      Expect.equal comps.normalV (vector 0. 0. -1.) ""
  ]
