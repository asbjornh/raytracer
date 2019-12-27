module CylinderTest

open Expecto

open Tuple
open Shape
open Ray
open Material
open Transform
open Util

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

[<Tests>]
let tests =
  testList "Tests for Cylinder" [
    testCase "A ray misses a cylinder" <| fun _ ->
      let test origin direction txt =
        let cyl = defaultOpenCylinder ()
        let direction = normalize direction
        let r = ray origin direction
        let xs = localIntersect r cyl
        Expect.equal (List.length xs) 0 txt

      test (point 1. 0. 0.) (vector 0. 1. 0.) "First"
      test (point 0. 0. 0.) (vector 0. 1. 0.) "Second"
      test (point 0. 0. -5.) (vector 1. 1. 1.) "Third"

    testCase "A ray strikes a cylinder" <| fun _ ->
      let test origin direction t0 t1 =
        let t = scale 1. 1000. 1.
        let cyl = openCylinder t <| defaultMaterial ()
        let direction = normalize direction
        let r = ray origin direction
        let xs = shapeIntersect r cyl
        Expect.equal (List.length xs) 2 ""
        let (localT0, _) = xs.[0]
        let (localT1, _) = xs.[1]
        Expect.isTrue (looseEq localT0 t0) <| diff localT0 t0
        Expect.isTrue (looseEq localT1 t1) <| diff localT1 t1

      test (point 1. 0. -5.) (vector 0. 0. 1.) 5. 5.
      test (point 0. 0. -5.) (vector 0. 0. 1.) 6. 4.
      test (point 0.5 0. -5.) (vector 0.1 1. 1.) 7.08872 6.80798

    testCase "Normal vector on a cylinder" <| fun _ ->
      let test point normal =
        let cyl = defaultOpenCylinder ()
        let n = localNormal cyl point
        Expect.equal n normal ""

      test (point 1. 0. 0.) (vector 1. 0. 0.)
      test (point 0. 5. -1.) (vector 0. 0. -1.)
      test (point 0. -2. 1.) (vector 0. 0. 1.)
      test (point -1. 1. 0.) (vector -1. 0. 0.)

    testCase "Intersecting a constrained cylinder" <| fun _ ->
      let test point direction count txt =
        let t = chain [
          translateY 1.5
          scaleY 0.5
        ]
        let cyl = openCylinder t <| defaultMaterial ()
        let direction = normalize direction
        let r = ray point direction
        let xs = shapeIntersect r cyl
        Expect.equal (List.length xs) count txt

      test (point 0. 1.5 0.) (vector 0.1 1. 0.) 0 "First"
      test (point 0. 3. -5.) (vector 0. 0. 1.) 0 "Second"
      test (point 0. 0. -5.) (vector 0. 0. 1.) 0 "Third"
      test (point 0. 2. -5.) (vector 0. 0. 1.) 0 "Fourth"
      test (point 0. 1. -5.) (vector 0. 0. 1.) 0 "Fifth"
      test (point 0. 1.5 -2.) (vector 0. 0. 1.) 2 "Sixth"

    testCase "Intersecting the caps of a closed cylinder" <| fun _ ->
      let test point direction count txt =
        let t = chain [
          translateY 1.5
          scaleY 0.5
        ]
        let cyl = cylinder t <| defaultMaterial ()
        let r = ray point <| normalize direction
        let xs = shapeIntersect r cyl
        Expect.equal (List.length xs) count txt

      test (point 0. 3. 0.) (vector 0. -1. 0.) 2 "First"
      test (point 0. 3. -2.) (vector 0. -1. 2.) 2 "Second"
      test (point 0. 4. -2.) (vector 0. -1. 1.) 2 "Third" //corner case​
      test (point 0. 0. -2.) (vector 0. 1. 2.) 2 "Fourth"
      test (point 0. -1. -2.) (vector 0. 1. 1.) 2 "Fifth" //corner case​

    testCase "The normal vector on a cylinder's end caps" <| fun _ ->
      let test point normal =
        let t = chain [
          translateY 1.5
          scaleY 0.5
        ]
        let cyl = cylinder t <| defaultMaterial ()
        let n = normalAt cyl point
        Expect.equal n normal ""

      test (point 0. 1. 0.) (vector 0. -1. 0.)
      test (point 0.5 1. 0.) (vector 0. -1. 0.)
      test (point 0. 1. 0.5) (vector 0. -1. 0.)
      test (point 0. 2. 0.) (vector 0. 1. 0.)
      test (point 0.5 2. 0.) (vector 0. 1. 0.)
      test (point 0. 2. 0.5) (vector 0. 1. 0.)
  ]
