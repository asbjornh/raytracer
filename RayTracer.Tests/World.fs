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
  Expect.isTrue (Color.equals actual expected) <| Expect.defaultDiffPrinter expected actual

[<Tests>]

let tests =
  testList "Tests for World" [
    testCase "Intersect a world with a ray" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let xs = intersect r w
      Expect.equal (List.length xs) 4 ""
      Expect.equal (xs.[0].t) 4.f "First t should be 4"
      Expect.equal (xs.[1].t) 4.5f "Second t should be 4.5"
      Expect.equal (xs.[2].t) 5.5f "Third t should be 5.5"
      Expect.equal (xs.[3].t) 6.f "Fourth t should be 6"

    testCase "Shading an intersection" <| fun _ ->
      let w = defaultWorld ()
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let shape = w.objects.[0]
      let i = intersection 4.f shape
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
      let i = intersection 0.5f shape
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
      outer.material <- Phong { defaultMaterialP with ambient = white }
      let inner = w.objects.[1]
      inner.material <- Phong { defaultMaterialP with ambient = white }
      let r = ray (point 0. 0. 0.75) (vector 0. 0. -1.)
      let c = colorAt w r 1
      let actual =
        match inner.material with
        | Phong m -> m.ambient
        | _ -> red
      Expect.equal c actual ""

    testCase "There is no shadow when nothing is collinear with point and light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 0. 10. 0.
      Expect.equal (shadowAmount false p w.lights.[0] w) 0. ""

    testCase "The shadow when an object is between the point and the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point 10. -10. 10.
      Expect.equal (shadowAmount false p w.lights.[0] w) 1. ""

    testCase "There is no shadow when an object is behind the light" <| fun _ ->
      let w = defaultWorld ()
      let p = point -20. 20. -20.
      Expect.equal (shadowAmount false p w.lights.[0] w) 0. ""

    testCase "There is no shadow when an object is behind the point" <| fun _ ->
      let w = defaultWorld ()
      let p = point -2. 2. -2.
      Expect.equal (shadowAmount false p w.lights.[0] w) 0. ""

    testCase "shade_hit() is given an intersection in shadow" <| fun _ ->
      let s = sphereT (translateZ 10.f)
      let w =
        world
        <| [pointLight (point 0. 0. -10.) (color 1. 1. 1.)]
        <| [unitSphere (); s]

      let r = ray (point 0. 0. 5.) (vector 0. 0. 1.)
      let i = intersection 4.f s
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.1 0.1 0.1) ""

    testCase "The reflected color for a reflective material" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        mode = Mix 0.5
        a = defaultMaterial
        b = Reflective None
      }
      let shape = plane (translateY -1.f) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.f) shape
      let comps = prepareComputations [i] i r
      let c = reflectedColor None w comps 1
      Expect.equal c (color 0.38066 0.47583 0.28549) ""

    testCase "shade_hit() with a reflective material" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        mode = Mix 0.5
        a = defaultMaterial
        b = Reflective None
      }
      let shape = plane (translateY -1.f) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.f) shape
      let comps = prepareComputations [i] i r
      let c = shadeHit w comps 1
      Expect.equal c (color 0.53354 0.58112 0.48596) ""

    testCase "color_at() with mutually reflective surfaces" <| fun _ ->
      let light = pointLight (point 0. 0. 0.) (color 1. 1. 1.)
      let mat = Reflective None
      let lower = plane (translateY -1.f) mat
      let upper = plane (translateY 1.f) mat
      let w = world [light] [lower; upper]
      let r = ray (point 0. 0. 0.) (vector 0. 1. 0.)
      Expect.equal (colorAt w r 1) black ""

    testCase "The reflected color at the maximum recursive depth" <| fun _ ->
      let w = defaultWorld ()
      let mat = Blend {
        mode = Mix 0.5
        a = defaultMaterial
        b = Reflective None
      }
      let shape = plane (translateY -1.f) mat
      let w = { w with objects = List.concat [w.objects; [shape]] }
      let a = (sqrt 2.) / 2.
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let i = intersection (sqrt 2.f) shape
      let comps = prepareComputations [i] i r
      let c = reflectedColor None w comps 0
      Expect.equal c black ""

    testCase "The refracted color at the maximum recursive depth" <| fun _ ->
      let w = defaultWorld ()
      let mat = Transparent { index = 1.5f }
      let shape = w.objects.[0]
      shape.material <- mat
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let xs = intersections [
        intersection 4.f shape
        intersection 6.f shape
      ]
      let comps = prepareComputations xs xs.[0] r
      let c = refractedColor w comps 0
      Expect.equal c black ""

    testCase "The refracted color under total internal reflection" <| fun _ ->
      let w = defaultWorld ()
      let mat = Transparent { index = 1.5f }
      let shape = w.objects.[0]
      shape.material <- mat
      let a = (sqrt 2.f) / 2.f
      let r = ray(point32 0.f 0.f a) (vector 0. 1. 0.)
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
      let matB = Transparent { index = 1.5f }
      let a = { w.objects.[0] with material = matA }
      let b = { w.objects.[1] with material = matB }
      let r = ray (point 0. 0. 0.1) (vector 0. 1. 0.)
      let xs = intersections [
        intersection -0.9899f a
        intersection -0.4899f b
        intersection 0.4899f b
        intersection 0.9899f a
      ]
      let w = { w with objects = [a; b] }
      let comps = prepareComputations xs xs.[2] r
      let c = refractedColor w comps 5
      expectColorEquals c (color 0. 0.99887 0.04721)

    testCase "shade_hit() with a transparent material" <| fun _ ->
      let w = defaultWorld ()
      let floor =
        plane <| chain [translateY -1.f; uniformScale 10.f]
        <| Blend {
          mode = Mix 0.5
          a = Blend {
            mode = Add
            a = defaultMaterial
            b = Transparent { index = 1.5f } }
          b = defaultMaterial
        }
      let ball =
        sphere <| translate 0.f -3.5f -0.5f
        <| material (color 1. 0. 0.) 0.5 0.9 0.9
      let w = { w with objects = [floor; ball] }
      let a = (sqrt 2. / 2.)
      let r = ray (point 0. 0. -3.) (vector 0. -a a)
      let xs = intersections [ intersection (sqrt 2.f) floor ]
      let comps = prepareComputations xs xs.[0] r
      let c = shadeHit w comps 5
      expectColorEquals c (color 0.93642 0.68642 0.68642)
  ]
