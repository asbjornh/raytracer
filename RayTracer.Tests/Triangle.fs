module TriangleTest

open Expecto

open Tuple
open Shape
open Ray
open Matrix
open Material


let testSmoothTri =
  let p1 = point 0. 1. 0.
  let p2 = point -1. 0. 0.
  let p3 = point 1. 0. 0.
  let n1 = vector 0. 1. 0.
  let n2 = vector -1. 0. 0.
  let n3 = vector 1. 0. 0.
  smoothTriangle p1 p2 p3 n1 n2 n3 identity <| defaultMaterial ()

[<Tests>]
let tests =
  testList "Tests for Triangle" [
    testCase "Finding the normal on a triangle" <| fun _ ->
      let p = Triangle.make <| point 0. 1. 0. <| point -1. 0. 0. <| point 1. 0. 0.
      let t = shapeT (Triangle p) identity
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
      Expect.equal (fst xs.[0]) 2.f ""

    testCase "An computation with a smooth triangle stores u/v" <| fun _ ->
      let r = ray <| point -0.2 0.3 -2. <| vector 0. 0. 1.
      let xs = Intersection.intersect r testSmoothTri
      let h = Intersection.hit xs |> Option.get
      let comps = Intersection.prepareComputations xs h r
      let (u, v) = Option.get comps.triangleUV
      Expect.equal u 0.45f ""
      Expect.equal v 0.25f ""

    testCase "Preparing the normal on a smooth triangle" <| fun _ ->
      let r = ray <| point -0.2 0.3 -2. <| vector 0. 0. 1.
      let xs = Intersection.intersect r testSmoothTri
      let h = Intersection.hit xs |> Option.get
      let comps = Intersection.prepareComputations xs h r
      Expect.equal comps.normalV (vector -0.5547 0.83205 0.) ""
  ]
