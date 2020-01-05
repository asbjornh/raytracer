module CubeTest

open Expecto

open Tuple
open Shape
open Ray

let testCubeIntersect origin direction t1 t2 =
  let c = unitCube ()
  let r = ray origin direction
  let xs = localIntersect r c
  let (localT1, _, _) = xs.[0]
  let (localT2, _, _) = xs.[1]
  Expect.equal (List.length xs) 2 ""
  Expect.equal localT1 t1 ""
  Expect.equal localT2 t2 ""

[<Tests>]
let tests =
  testList "Tests for Cube" [
    testCase "Cube intersection +x" <| fun _ ->
      testCubeIntersect (point 5. 0.5 0.) (vector -1. 0. 0.) 4.f 6.f

    testCase "Cube intersection -x" <| fun _ ->
      testCubeIntersect (point -5. 0.5 0.) (vector 1. 0. 0.) 4.f 6.f

    testCase "Cube intersection +y" <| fun _ ->
      testCubeIntersect (point 0.5 5. 0.) (vector 0. -1. 0.) 4.f 6.f

    testCase "Cube intersection -y" <| fun _ ->
      testCubeIntersect (point 0.5 -5. 0.) (vector 0. 1. 0.) 4.f 6.f

    testCase "Cube intersection +z" <| fun _ ->
      testCubeIntersect (point 0.5 0. 5.) (vector 0. 0. -1.) 4.f 6.f

    testCase "Cube intersection -z" <| fun _ ->
      testCubeIntersect (point 0.5 0. -5.) (vector 0. 0. 1.) 4.f 6.f

    testCase "Cube intersection inside" <| fun _ ->
      testCubeIntersect (point 0. 0.5 0.) (vector 0. 0. 1.) -1.f 1.f

    testCase "Cube ray not intersecting" <| fun _ ->
      let testCubeMiss origin direction txt =
        let c = unitCube ()
        let r = ray origin direction
        let xs = localIntersect r c
        Expect.equal (List.length xs) 0 txt

      testCubeMiss (point -2. 0. 0.) (vector 0.2673 0.5345 0.8018) "First"
      testCubeMiss (point 0. -2. 0.) (vector 0.8018 0.2673 0.5345) "Second"
      testCubeMiss (point 0. 0. -2.) (vector 0.5345 0.8018 0.2673) "Third"
      testCubeMiss (point 2. 0. 2.) (vector 0. 0. -1.) "Fifth"
      testCubeMiss (point 0. 2. 2.) (vector 0. -1. 0.) "Sixth"
      testCubeMiss (point 2. 2. 0.) (vector -1. 0. 0.) "Seventh"

    testCase "The normal on the surface of a cube" <| fun _ ->
      let test point normal txt =
        let c = unitCube ()
        let n = localNormal c point
        Expect.equal n normal txt

      test (point 1. 0.5 -0.8) (vector 1. 0. 0.) "First"
      test (point -1. -0.2 0.9) (vector -1. 0. 0.) "Second"
      test (point -0.4 1. -0.1) (vector 0. 1. 0.) "Third"
      test (point 0.3 -1. -0.7) (vector 0. -1. 0.) "Fourth"
      test (point -0.6 0.3 1.) (vector 0. 0. 1.) "Fifth"
      test (point 0.4 0.4 -1.) (vector 0. 0. -1.) "Sixth"
      test (point 1. 1. 1.) (vector 1. 0. 0.) "Seventh"
      test (point -1. -1. -1.) (vector -1. 0. 0.) "Eight"
  ]
