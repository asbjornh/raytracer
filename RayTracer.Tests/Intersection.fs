module IntersectionTest

open Expecto

open Color
open Intersection
open Material
open Ray
open Shape
open Tuple
open Transform
open Util

let glassSphere index t =
  sphere t <| Transparent { index = index; blend = Normal }

let testRefraction index n1 n2 =
  let a = glassSphere 1.5f <| uniformScale 2.f
  let b = glassSphere 2.0f <| translateZ -0.25f
  let c = glassSphere 2.5f <| translateZ 0.25f
  let r = ray (point 0. 0. -4.) (vector 0. 0. 1.)
  let xs = intersections [
    intersection 2.f a
    intersection 2.75f b
    intersection 3.25f c
    intersection 4.75f b
    intersection 5.25f c
    intersection 6.f a
  ]
  let comps = prepareComputations xs xs.[index] r
  Expect.equal comps.n1 n1 ""
  Expect.equal comps.n2 n2 ""

[<Tests>]
let tests =
  testList "Tests for Intersection" [
    testCase "An intersection encapsulates t and object" <| fun _ ->
      let s = unitSphere ()
      let i = intersection 3.5f s
      Expect.equal i.t 3.5f ""
      Expect.equal i.object s ""

    testCase "The hit, when all intersections have positive t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection 1.f s
      let i2 = intersection 2.f s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i1) ""

    testCase "The hit, when some intersections have negative t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection -1.f s
      let i2 = intersection 1.f s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) (Some i2) ""

    testCase "The hit, when all intersections have negative t" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection -2.f s
      let i2 = intersection -1.f s
      let xs = intersections [i2; i1]
      Expect.equal (hit xs) None ""

    testCase "The hit is always the lowest nonnegative intersection" <| fun _ ->
      let s = unitSphere ()
      let i1 = intersection 5.f s
      let i2 = intersection 7.f s
      let i3 = intersection -3.f s
      let i4 = intersection 2.f s
      let xs = intersections [i1; i2; i3; i4]
      Expect.equal (hit xs) (Some i4) ""

    testCase "Precomputing the state of an intersection" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 4.f shape
      let comps = prepareComputations [i] i r
      Expect.equal comps.t i.t ""
      Expect.equal comps.object i.object ""
      Expect.equal comps.point (point 0. 0. -1.) ""
      Expect.equal comps.eyeV (vector 0. 0. -1.) ""
      Expect.equal comps.normalV (vector 0. 0. -1.) ""

    testCase "The hit, when an intersection occurs on the outside" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 4.f shape
      let comps = prepareComputations [i] i r
      Expect.equal comps.inside false ""

    testCase "The hit, when an intersection occurs on the inside" <| fun _ ->
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let shape = unitSphere ()
      let i = intersection 1.f shape
      let comps = prepareComputations [i] i r
      Expect.equal comps.point (point 0. 0. 1.) ""
      Expect.equal comps.eyeV (vector 0. 0. -1.) ""
      Expect.equal comps.inside true ""
      // normal would have been (0, 0, 1), but is inverted!​
      Expect.equal comps.normalV (vector 0. 0. -1.) ""

    testCase "The hit should offset the point" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = sphereT (translate 0.f 0.f 1.f)
      let i = intersection 5.f shape
      let comps = prepareComputations [i] i r
      Expect.isLessThan comps.overPoint.Z (-epsilon32 / 2.f) ""
      Expect.isGreaterThan comps.point.Z comps.overPoint.Z ""

    testCase "Precomputing the reflection vector" <| fun _ ->
      let shape = defaultPlane ()
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 1. -1.) (vector 0. -a a) 
      let i = intersection (sqrt 2.f) shape
      let comps = prepareComputations [i] i r
      Expect.equal comps.reflectV (vector 0. a a) ""

    testCase "Refraction 0" <| fun _ ->
      testRefraction 0 1.0f 1.5f

    testCase "Refraction 1" <| fun _ ->
      testRefraction 1 1.5f 2.0f

    testCase "Refraction 2" <| fun _ ->
      testRefraction 2 2.0f 2.5f

    testCase "Refraction 3" <| fun _ ->
      testRefraction 3 2.5f 2.5f

    testCase "Refraction 4" <| fun _ ->
      testRefraction 4 2.5f 1.5f

    testCase "Refraction 5" <| fun _ ->
      testRefraction 5 1.5f 1.0f

    testCase "The under point is offset below the surface" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = glassSphere 1.5f <| translate 0.f 0.f 1.f
      let i = intersection 5.f shape
      let comps = prepareComputations [i] i r
      Expect.isGreaterThan comps.underPoint.Z (epsilon32 / 2.f) ""
      Expect.isLessThan comps.point.Z comps.underPoint.Z ""
  ]
