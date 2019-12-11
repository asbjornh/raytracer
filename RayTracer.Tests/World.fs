module WorldTest

open Expecto

open Color
open Light
open Material
open Ray
open Shape
open Tuple
open Transform
open World

let defaultWorld () =
  let mat = {
    material () with
      color = color 0.8 1. 0.6;
      diffuse = 0.7
      specular = 0.2
  }
  {
    light = pointLight (point -10. 10. -10.) (color 1. 1. 1.);
    objects = [
      sphereM mat
      sphereT (scaling 0.5 0.5 0.5)
    ]
  }

[<Tests>]

let tests =
  testList "Tests for World" [
    testCase "The default world" <| fun _ ->
      let light = pointLight (point -10. 10. -10.) (color 1. 1. 1.)
      let m1 = {
        material () with
          color = color 0.8 1. 0.6;
          diffuse = 0.7
          specular = 0.2
      }

      let s1 = sphereM m1
      let s2 = sphereT (scaling 0.5 0.5 0.5)
      let w = defaultWorld ()
      Expect.equal w.light light ""
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
  ]
