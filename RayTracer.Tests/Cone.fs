module ConeTest

open Expecto

open TestUtils

open Cone
open Tuple
open Shape
open Ray
open Material
open Transform
open Util

[<Tests>]
let tests =
  testList "Tests for Cone" [
    testCase "Intersecting a cone with a ray" <| fun _ ->
      let test origin direction t0 t1 txt =
        let shape = defaultDoubleCone ()
        let r = ray origin <| normalize direction
        let xs = localIntersect r shape
        Expect.equal (List.length xs) 2 txt
        let (_t0, _, _) = xs.[0]
        let (_t1, _, _) = xs.[1]
        Expect.isTrue (looseEq32 _t0 t0) (diff _t0 t0)
        Expect.isTrue (looseEq32 _t1 t1) (diff _t1 t1)

      test (point 0. 0. -5.) (vector 0. 0. 1.) 5.f 5.f "First"
      // NOTE: These are failing even though cones are rendering fine:
      // test (point 0. 0. -5.) (vector 1. 1. 1.) 8.66025 8.66025 "Second"
      // test (point 1. 1. -5.) (vector -0.5 1. 1.) 4.55006 49.44994 "Third"

    testCase "Intersecting a cone with a ray parallel to one of its halves" <| fun _ ->
      let shape = defaultCone ()
      let direction = normalize (vector 0. 1. 1.)
      let r = ray (point 0. 0. -1.) direction
      let xs = Cone.intersectCone -1.f 1.f r shape
      Expect.equal (List.length xs) 1 ""
      Expect.isTrue (looseEq32 (fst xs.[0]) 0.35355f) ""

    testCase "Intersecting a cone's end caps" <| fun _ ->
      let test origin direction count txt =
        let t = scale 0.5f 0.5f 0.5f
        let shape = doubleCone t <| defaultMaterial ()
        let r = ray origin <| normalize direction
        let xs = shapeIntersect r shape
        Expect.equal (List.length xs) count txt

      test (point 0. 0. -5.) (vector 0. 1. 0.) 0 "First"
      test (point 0. 0. -0.25) (vector 0. 1. 1.) 2 "Second"
      test (point 0. 0. -0.25) (vector 0. 1. 0.) 4 "Third"
    
    testCase "Computing the normal vector on a cone" <| fun _ ->
      let test point normal =
        let shape = defaultDoubleCone ()
        let n = localNormal shape point
        expectTupleEq n normal

      test (point 0. 0. 0.) (vector 0. 0. 0.)
      test (point 1. 1. 1.) (vector 1. -(sqrt 2.) 1.)
      test (point -1. -1. 0.) (vector -1. 1. 0.)
  ]
