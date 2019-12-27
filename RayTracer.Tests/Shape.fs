module ShapeTest

open System
open Expecto

open Tuple
open Shape
open Ray
open Intersection
open Matrix
open Material
open Transform
open Util

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

let testShape t = {
  transform = t
  material = Material.defaultMaterial ()
  shape = TestShape
  parent = None
  children = []
}

[<Tests>]
let tests =
  testList "Tests for Shape" [
    testCase "Computing the normal on a translated shape" <| fun _ ->
      let s = testShape (translate 0. 1. 0.)
      let n = normalAt s (point 0. 1.70711 -0.70711)
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed shape" <| fun _ ->
      let s =
        testShape
        <| chain [scale 1. 0.5 1.; rotateZ (Math.PI / 5.)]
      let a = (sqrt 2.) / 2.
      let n = normalAt s (point 0. a -a)
      Expect.equal n (vector 0. 0.97014 -0.24254) ""

    testCase "Finding the normal on a triangle" <| fun _ ->
      let p = polyP <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let t = shapeT (Poly p) <| identity ()
      let n1 = localNormal t <| point 0. 0.5 0.
      let n2 = localNormal t <| point -0.5 0.75 0.
      let n3 = localNormal t <| point 0.5 0.25 0.
      Expect.equal n1 p.normal ""
      Expect.equal n2 p.normal ""
      Expect.equal n3 p.normal ""

    testCase "Intersecting a ray parallel to the triangle" <| fun _ ->
      let p = defaultPoly <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. -1. -2. <| vector 0. 1. 0.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p1-p3 edge" <| fun _ ->
      let p = defaultPoly <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 1. 1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p1-p2 edge" <| fun _ ->
      let p = defaultPoly <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point -1. 1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p2-p3 edge" <| fun _ ->
      let p = defaultPoly <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. -1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray strikes a triangle" <| fun _ ->
      let p = defaultPoly <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. 0.5 -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      Expect.equal (fst xs.[0]) 2. ""
  ]
