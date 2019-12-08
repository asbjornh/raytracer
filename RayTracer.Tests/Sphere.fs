module SphereTest

open System

open Expecto
open Tuple
open Sphere
open Ray
open Intersection
open Matrix
open Material
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

    testCase "Intersecting a scaled sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let s2 = Sphere.transform (scaling 2. 2. 2.) s
      let xs = intersect s2 r
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 3. ""
      Expect.equal xs.[1].t 7. ""

    testCase "Intersecting a translated sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere ()
      let s2 = Sphere.transform (translation 5. 0. 0.) s
      let xs = intersect s2 r
      Expect.equal (List.length xs) 0 ""

    testCase "The normal on a sphere at a point on the x axis" <| fun _ ->
      let s = sphere ()
      let n = normal (point 1. 0. 0.) s
      Expect.equal n (vector 1. 0. 0.) ""

    testCase "The normal on a sphere at a point on the y axis" <| fun _ ->
      let s = sphere ()
      let n = normal (point 0. 1. 0.) s
      Expect.equal n (vector 0. 1. 0.) ""

    testCase "The normal on a sphere at a point on the z axis" <| fun _ ->
      let s = sphere ()
      let n = normal (point 0. 0. 1.) s
      Expect.equal n (vector 0. 0. 1.) ""

    testCase "The normal on a sphere at a nonaxial point" <| fun _ ->
      let s = sphere ()
      let a = (sqrt 3.) / 3.
      let n = normal (point a a a) s
      Expect.equal n (vector a a a) ""

    testCase "The normal is a normalized vector" <| fun _ ->
      let s = sphere ()
      let a = (sqrt 3.) / 3.
      let n = normal (point a a a) s
      Expect.equal n (normalize n) ""

    testCase "Computing the normal on a translated sphere" <| fun _ ->
      let s = sphere ()
      let s2 = Sphere.transform (translation 0. 1. 0.) s
      let n = normal (point 0. 1.70711 -0.70711) s2
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed sphere" <| fun _ ->
      let diff actual expected = Expect.defaultDiffPrinter expected actual
      let s = sphere ()
      let m = (scaling 1. 0.5 1.) * (rotationZ (Math.PI / 5.))
      let s2 = Sphere.transform m s
      let a = (sqrt 2.) / 2.
      let n = normal (point 0. a -a) s2
      Expect.equal n (vector 0. 0.97014 -0.24254) ""

    testCase "A sphere has a default material" <| fun _ ->
      let s = sphere ()
      let m = s.material
      Expect.equal m (material ()) ""

    testCase "A sphere may be assigned a material" <| fun _ ->
      let s = sphere ()
      let m = material ()
      m.ambient <- 1.
      s.material <- m
      Expect.equal s.material m ""
  ]
