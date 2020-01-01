module PolyTest

open Expecto

open Tuple
open Shape
open Ray
open Matrix

[<Tests>]
let tests =
  testList "Tests for Poly" [
    testCase "Finding the normal on a triangle" <| fun _ ->
      let p = Triangle.make <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let t = shapeT (Triangle p) <| identity ()
      let n1 = localNormal t <| point 0. 0.5 0.
      let n2 = localNormal t <| point -0.5 0.75 0.
      let n3 = localNormal t <| point 0.5 0.25 0.
      Expect.equal n1 p.normal ""
      Expect.equal n2 p.normal ""
      Expect.equal n3 p.normal ""

    testCase "Intersecting a ray parallel to the triangle" <| fun _ ->
      let p = defaultTriangle <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. -1. -2. <| vector 0. 1. 0.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p1-p3 edge" <| fun _ ->
      let p = defaultTriangle <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 1. 1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p1-p2 edge" <| fun _ ->
      let p = defaultTriangle <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point -1. 1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray misses the p2-p3 edge" <| fun _ ->
      let p = defaultTriangle <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. -1. -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 0 ""

    testCase "A ray strikes a triangle" <| fun _ ->
      let p = defaultTriangle <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let r = ray <| point 0. 0.5 -2. <| vector 0. 0. 1.
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      Expect.equal (fst xs.[0]) 2. ""
  ]
