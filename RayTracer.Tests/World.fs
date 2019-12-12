module WorldTest

open Expecto

open Color
open Intersection
open Light
open Material
open Ray
open Shape
open Tuple
open Transform
open World

[<Tests>]

let tests =
  testList "Tests for World" [
    testCase "The default world" <| fun _ ->
      let light = pointLight (point -10. 10. -10.) (color 1. 1. 1.)
      let m1 = material (color 0.8 1. 0.6) 0.1 0.7 0.2

      let s1 = sphereM m1
      let s2 = sphereT (scaling 0.5 0.5 0.5)
      let w = defaultWorld ()
      Expect.equal w.lights.[0] light ""
      Expect.contains w.objects (s1 :> IShape) ""
      Expect.contains w.objects (s2 :> IShape) ""

    testCase "Intersect a world with a ray" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let xs = intersect r w
      Expect.equal (List.length xs) 4 ""
      Expect.equal (xs.[0].t) 4. "First t should be 4"
      Expect.equal (xs.[1].t) 4.5 "Second t should be 4.5"
      Expect.equal (xs.[2].t) 5.5 "Third t should be 5.5"
      Expect.equal (xs.[3].t) 6. "Fourth t should be 6"

    testCase "Shading an intersection" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = w.objects.[0]
      let i = intersection 4. shape
      let comps = prepareComputations i r
      let c = shadeHit w comps
      Expect.equal c (color 0.38066 0.47583 0.2855) ""

    testCase "Shading an intersection from the inside" <| fun _ ->
      let w = {
         defaultWorld () with 
           lights = [pointLight (point 0. 0.25 0.) (color 1. 1. 1.)]
      }
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let shape = w.objects.[1]
      let i = intersection 0.5 shape
      let comps = prepareComputations i r
      let c = shadeHit w comps
      Expect.equal c (color 0.90498 0.90498 0.90498) ""

    testCase "The color when a ray misses" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 1. 0.)
      let c = colorAt w r
      Expect.equal c (color 0. 0. 0.) ""

    testCase "The color when a ray hits" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let c = colorAt w r
      Expect.equal c (color 0.38066 0.47583 0.2855) ""

    testCase "The color with an intersection behind the ray" <| fun _ ->
      let w = defaultWorld ()
      let outer = w.objects.[0]
      outer.Material.ambient <- 1.
      let inner = w.objects.[1]
      inner.Material.ambient <- 1.
      let r = ray (point 0. 0. 0.75) (vector 0. 0. -1.)
      let c = colorAt w r
      Expect.equal c inner.Material.color ""

    testCase "There is no shadow when nothing is collinear with point and light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 0. 10. 0.
      Expect.equal (isInShadow p w.lights.[0] w.objects) false ""

    testCase "The shadow when an object is between the point and the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 10. -10. 10.
      Expect.equal (isInShadow p w.lights.[0] w.objects) true ""

    testCase "There is no shadow when an object is behind the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point -20. 20. -20.
      Expect.equal (isInShadow p w.lights.[0] w.objects) false ""

    testCase "There is no shadow when an object is behind the point" <| fun _ ->
      let w = defaultWorld ()
      let p = point -2. 2. -2.
      Expect.equal (isInShadow p w.lights.[0] w.objects) false ""
  ]
