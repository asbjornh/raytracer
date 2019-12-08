module IntersectionTest

open Expecto
open Sphere
open Intersection

[<Tests>]
let tests =
  testList "Tests for Intersection" [
    testCase "An intersection encapsulates t and object" <| fun _ ->
      let s = sphere ()
      let i = intersection 3.5 s
      Expect.equal i.t 3.5 ""
      Expect.equal i.object s ""

    testCase "The hit, when all intersections have positive t" <| fun _ ->
      let s = sphere ()
      let i1 = intersection 1. s
      let i2 = intersection 2. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i1) ""

    testCase "The hit, when some intersections have negative t" <| fun _ ->
      let s = sphere ()
      let i1 = intersection -1. s
      let i2 = intersection 1. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i2) ""

    testCase "The hit, when all intersections have negative t" <| fun _ ->
      let s = sphere ()
      let i1 = intersection -2. s
      let i2 = intersection -1. s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) None ""

    testCase "The hit is always the lowest nonnegative intersection" <| fun _ ->
      let s = sphere ()
      let i1 = intersection 5. s
      let i2 = intersection 7. s
      let i3 = intersection -3. s
      let i4 = intersection 2. s
      let xs = intersections [i1; i2; i3; i4]
      Expect.equal (hit xs) (Some i4) ""
  ]
