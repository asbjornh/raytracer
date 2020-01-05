module GroupTest

open System
open Expecto

open Color
open Tuple
open Shape
open Ray
open Material
open Matrix
open Transform

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

[<Tests>]
let tests =
  testList "Tests for Group" [
    testCase "Intersecting a ray with an empty group" <| fun _ ->
      let g = groupT [] <| identity
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let xs = localIntersect r g
      Expect.equal (List.length xs) 0 ""

    testCase "Intersecting a ray with a nonempty group" <| fun _ ->
      let s1 = unitSphere ()
      let s2 = sphereT (translate 0.f 0.f -3.f)
      let s3 = sphereT (translate 5.f 0.f 0.f)
      let g = groupT [s1; s2; s3] <| identity
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let xs = localIntersect r g
      let (_, objects, _) = List.unzip3 xs
      Expect.equal (List.length xs) 4 ""
      // NOTE: s1 and s2 do not have a reference to their parent since the group isn't created until after they're initialized
      Expect.equal { objects.[0] with parent = None } s1 ""
      Expect.equal { objects.[1] with parent = None } s1 ""
      Expect.equal { objects.[2] with parent = None } s2 ""
      Expect.equal { objects.[3] with parent = None } s2 ""

    testCase "Intersecting a transformed group" <| fun _ ->
      let s = sphereT <| translate 5.f 0.f 0.f
      let g = groupT [s] <| scale 2.f 2.f 2.f
      let r = ray (point 10. 0. -10.) (vector 0. 0. 1.)
      let xs = shapeIntersect r g
      Expect.equal (List.length xs) 2 ""

    testCase "Converting a point from world to object space" <| fun _ ->
      let s = sphereT <| translate 5.f 0.f 0.f
      let g2 = groupT [s] <| scale 2.f 2.f 2.F
      let g1 = groupT [g2] (rotateY <| MathF.PI / 2.f)

      // NOTE: Reassign s to its corresponding element in the group to get the sphere with references to the parent groups
      let s = g1.children.[0].children.[0]
      let p = worldToObject s (point -2. 0. -10.)
      Expect.equal p (point 0. 0. -1.) ""

    testCase "Converting a normal from object to world space" <| fun _ ->
      let s = sphereT <| translate 5.f 0.f 0.f
      let g2 = groupT [s] <| scale 1.f 2.f 3.f
      let g1 = groupT [g2] <| rotateY (MathF.PI / 2.f)
      let a = (sqrt 3.) / 3.

      // NOTE: Reassign s to its corresponding element in the group to get the sphere with references to the parent groups
      let s = g1.children.[0].children.[0]
      let n = normalToWorld s (vector a a a)
      Expect.equal n (vector 0.28571 0.42857 -0.85714) ""

    testCase "Finding the normal on a child object" <| fun _ ->
      let s = sphereT <| translate 5.f 0.f 0.f
      let g2 = groupT [s] <| scale 1.f 2.f 3.f
      let g1 = groupT [g2] <| rotateY (MathF.PI / 2.f)

      // NOTE: Reassign s to its corresponding element in the group to get the sphere with references to the parent groups
      let s = g1.children.[0].children.[0]
      let n = normalAt s (point 1.7321 1.1547 -5.5774)
      Expect.equal n (vector 0.2857 0.42854 -0.85716) ""
  ]
