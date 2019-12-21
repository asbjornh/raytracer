module WorldTest

open Expecto

open Color
open Intersection
open Light
open Material
open Matrix
open Pattern
open Ray
open Shape
open Tuple
open Transform
open World

let expectColorEquals actual expected =
  Expect.isTrue (equals actual expected) <| Expect.defaultDiffPrinter expected actual

[<Tests>]

let tests =
  testList "Tests for World" [
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
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.38066 0.47583 0.2855) ""

    testCase "Shading an intersection from the inside" <| fun _ ->
      let w = {
         defaultWorld () with 
           lights = [pointLight (point 0. 0.25 0.) (color 1. 1. 1.)]
      }
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let shape = w.objects.[1]
      let i = intersection 0.5 shape
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.90498 0.90498 0.90498) ""

    testCase "The color when a ray misses" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 1. 0.)
      let c = colorAt w r 1
      Expect.equal c (color 0. 0. 0.) ""

    testCase "The color when a ray hits" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let c = colorAt w r 1
      Expect.equal c (color 0.38066 0.47583 0.2855) ""

    testCase "The color with an intersection behind the ray" <| fun _ ->
      let w = defaultWorld ()
      let outer = w.objects.[0]
      outer.material <- Phong { defaultMaterialP () with ambient = 1. }
      let inner = w.objects.[1]
      inner.material <- Phong { defaultMaterialP () with ambient = 1. }
      let r = ray (point 0. 0. 0.75) (vector 0. 0. -1.)
      let c = colorAt w r 1
      let actual =
        match inner.material with
        | Phong m -> m.color
        | _ -> red
      Expect.equal c actual ""

    testCase "There is no shadow when nothing is collinear with point and light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 0. 10. 0.
      Expect.equal (shadowAmount p w.lights.[0] w.objects) 0. ""

    testCase "The shadow when an object is between the point and the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 10. -10. 10.
      Expect.equal (shadowAmount p w.lights.[0] w.objects) 1. ""

    testCase "There is no shadow when an object is behind the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point -20. 20. -20.
      Expect.equal (shadowAmount p w.lights.[0] w.objects) 0. ""

    testCase "There is no shadow when an object is behind the point" <| fun _ ->
      let w = defaultWorld ()
      let p = point -2. 2. -2.
      Expect.equal (shadowAmount p w.lights.[0] w.objects) 0. ""

    testCase "shade_hit() is given an intersection in shadow" <| fun _ ->
      let s = sphereT (translation 0. 0. 10.)
      let w =
        world
        <| [pointLight (point 0. 0. -10.) (color 1. 1. 1.)]
        <| [unitSphere (); s]

      let r = ray (point 0. 0. 5.) (vector 0. 0. 1.)
      let i = intersection 4. s
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.1 0.1 0.1) ""

    testCase "The reflected color for a reflective material" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        a = defaultMaterial ()
        b = Reflective { blend = Normal }
        mix = 0.5
      }
      let shape = plane (translation 0. -1. 0.) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.) shape
      let comps = prepareComputations [i] i r
      let c = reflectedColor w comps 1
      Expect.equal c (color 0.38066 0.47583 0.28549) ""

    testCase "shade_hit() with a reflective material" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        a = defaultMaterial ()
        b = Reflective { blend = Normal }
        mix = 0.5
      }
      let shape = plane (translation 0. -1. 0.) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.) shape
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.53354 0.58112 0.48596) ""

    testCase "color_at() with mutually reflective surfaces" <| fun _ ->
      let light = pointLight (point 0. 0. 0.) (color 1. 1. 1.)
      let mat = Reflective { blend = Normal }
      let lower = plane (translation 0. -1. 0.) mat
      let upper = plane (translation 0. 1. 0.) mat
      let w = world [light] [lower; upper]
      let r = ray (point 0. 0. 0.) (vector 0. 1. 0.)
      Expect.equal (colorAt w r 1) black ""

    testCase "The reflected color at the maximum recursive depth" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        a = defaultMaterial ()
        b = Reflective { blend = Normal }
        mix = 0.5
      }
      let shape = plane (translation 0. -1. 0.) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.) shape
      let comps = prepareComputations [i] i r
      let c = reflectedColor w comps 0
      Expect.equal c black ""

    testCase "The refracted color at the maximum recursive depth" <| fun _ ->
      let w = defaultWorld ()
      let mat = Transparent { index = 1.5; blend = Normal }
      let shape = w.objects.[0]
      shape.material <- mat
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let xs = intersections [
        intersection 4. shape
        intersection 6. shape
      ]
      let comps = prepareComputations xs xs.[0] r
      let c = refractedColor w comps 0
      Expect.equal c black ""

    testCase "The refracted color under total internal reflection" <| fun _ ->
      let w = defaultWorld ()
      let mat = Transparent { index = 1.5; blend = Normal }
      let shape = w.objects.[0]
      shape.material <- mat
      let a = (sqrt 2.) / 2.
      let r = ray(point 0. 0. a) (vector 0. 1. 0.)
      let xs = intersections [
        intersection -a shape
        intersection a shape
      ]
      // NOTE: this time you're inside the sphere, so you need​
      // to look at the second intersection, xs[1], not xs[0]​
      let comps = prepareComputations xs xs.[1] r
      let c = refractedColor w comps 5
      Expect.equal c black ""

    testCase "The refracted color with a refracted ray" <| fun _ ->
      let w = defaultWorld ()
      let matA = TestPattern
      let matB = Transparent { index = 1.5; blend = Normal }
      let a = { w.objects.[0] with material = matA }
      let b = { w.objects.[1] with material = matB }
      let r = ray (point 0. 0. 0.1) (vector 0. 1. 0.)
      let xs = intersections [
        intersection -0.9899 a
        intersection -0.4899 b
        intersection 0.4899 b
        intersection 0.9899 a
      ]
      let w = { w with objects = [a; b] }
      let comps = prepareComputations xs xs.[2] r
      let c = refractedColor w comps 5
      expectColorEquals c (color 0. 0.99887 0.04721)

    testCase "shade_hit() with a transparent material" <| fun _ ->
      let w = defaultWorld ()
      let floor =
        plane <| translation 0. -1. 0.
        <| Blend {
          a = Transparent { index = 1.5; blend = Add }
          b = defaultMaterial ()
          mix = 0.5
        }
      let ball =
        sphere <| translation 0. -3.5 -0.5
        <| material (color 1. 0. 0.) 0.5 0.9 0.9
      let w = { w with objects = [floor; ball] }
      let a = (sqrt 2. / 2.)
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let xs = intersections [ intersection (sqrt 2.) floor ]
      let comps = prepareComputations xs xs.[0] r
      let c = shadeHit w comps 5
      expectColorEquals c (color 0.93642 0.68642 0.68642)
  ]
