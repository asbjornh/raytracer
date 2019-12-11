module SphereTest

open System

open Expecto
open Tuple
open Shape
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
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 4. ""
      Expect.equal xs.[1].t 6. ""

    testCase "A ray intersects a sphere at a tangent" <| fun _ ->
      let r = ray (point 0. 1. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 5. ""
      Expect.equal xs.[1].t 5. ""

    testCase "A ray misses a sphere" <| fun _ ->
      let r = ray (point 0. 2. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 0 ""

    testCase "A ray originates inside a sphere" <| fun _ ->
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -1. ""
      Expect.equal xs.[1].t 1. ""

    testCase "A sphere is behind a ray" <| fun _ ->
      let r = ray (point 0. 0. 5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -6. ""
      Expect.equal xs.[1].t -4. ""

    testCase "Intersect sets the object on the intersection" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal (xs.[0].object) (s :> IShape) ""
      Expect.equal (xs.[1].object) (s :> IShape) ""

    testCase "A Sphere's default transformation" <| fun _ ->
      let s = unitSphere ()
      Expect.equal s.Transform (identity ()) ""

    testCase "Changing a sphere's transformation" <| fun _ ->
      let t = translation 2. 3. 4.
      let s = sphere t (material ())
      Expect.equal s.Transform t ""

    testCase "Intersecting a scaled sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere (scaling 2. 2. 2.) (material ())
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 3. ""
      Expect.equal xs.[1].t 7. ""

    testCase "Intersecting a translated sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere (translation 5. 0. 0.) (material ())
      let xs = intersect r s
      Expect.equal (List.length xs) 0 ""

    testCase "The normal on a sphere at a point on the x axis" <| fun _ ->
      let s = unitSphere ()
      let n = normal (point 1. 0. 0.) s
      Expect.equal n (vector 1. 0. 0.) ""

    testCase "The normal on a sphere at a point on the y axis" <| fun _ ->
      let s = unitSphere ()
      let n = normal (point 0. 1. 0.) s
      Expect.equal n (vector 0. 1. 0.) ""

    testCase "The normal on a sphere at a point on the z axis" <| fun _ ->
      let s = unitSphere ()
      let n = normal (point 0. 0. 1.) s
      Expect.equal n (vector 0. 0. 1.) ""

    testCase "The normal on a sphere at a nonaxial point" <| fun _ ->
      let s = unitSphere ()
      let a = (sqrt 3.) / 3.
      let n = normal (point a a a) s
      Expect.equal n (vector a a a) ""

    testCase "The normal is a normalized vector" <| fun _ ->
      let s = unitSphere ()
      let a = (sqrt 3.) / 3.
      let n = normal (point a a a) s
      Expect.equal n (normalize n) ""

    testCase "Computing the normal on a translated sphere" <| fun _ ->
      let s = sphere (translation 0. 1. 0.) (material ())
      let n = normal (point 0. 1.70711 -0.70711) s
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed sphere" <| fun _ ->
      let m = (scaling 1. 0.5 1.) * (rotationZ (Math.PI / 5.))
      let s = sphere m (material ())
      let a = (sqrt 2.) / 2.
      let n = normal (point 0. a -a) s
      Expect.equal n (vector 0. 0.97014 -0.24254) ""

    testCase "A sphere has a default material" <| fun _ ->
      let s = unitSphere ()
      let m = s.Material
      Expect.equal m (material ()) ""

    testCase "A sphere may be assigned a material" <| fun _ ->
      let s = unitSphere ()
      let m = material ()
      let s2 = { s with Material = m}
      Expect.equal s2.Material m ""
  ]
