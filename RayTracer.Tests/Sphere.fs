module SphereTest

open Expecto
open Tuple
open Sphere
open Ray
open Intersection
open Matrix
open Transform

[<Tests>]
let tests =
  testList "Tests for Sphere" [
    testCase "A ray intersects a sphere at two points" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 4. ""
      Expect.equal xs.[1].t 6. ""

    testCase "A ray intersects a sphere at a tangent" <| fun _ ->
      let r = ray (point 0. 1. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 5. ""
      Expect.equal xs.[1].t 5. ""

    testCase "A ray misses a sphere" <| fun _ ->
      let r = ray (point 0. 2. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 0 ""

    testCase "A ray originates inside a sphere" <| fun _ ->
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -1. ""
      Expect.equal xs.[1].t 1. ""

    testCase "A sphere is behind a ray" <| fun _ ->
      let r = ray (point 0. 0. 5.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -6. ""
      Expect.equal xs.[1].t -4. ""

    testCase "Intersect sets the object on the intersection" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let xs = intersect s r
      Expect.equal (List.length xs) 2 ""
      Expect.equal (xs.[0].object) s ""
      Expect.equal (xs.[1].object) s ""

    testCase "A Sphere's default transformation" <| fun _ ->
      let s = sphere ()
      Expect.equal s.transform (identity ()) ""

    testCase "Changing a sphere's transformation" <| fun _ ->
      let s = sphere ()
      let t = translation 2. 3. 4.
      let s2 = Sphere.transform t s
      Expect.equal s2.transform t ""
  ]
