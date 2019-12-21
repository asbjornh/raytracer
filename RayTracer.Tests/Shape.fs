module ShapeTest

open System
open Expecto

open Tuple
open Shape
open Ray
open Intersection
open Matrix
open Material
open Transform
open Util

let diff actual expected =
  Expect.defaultDiffPrinter expected actual

let testCubeIntersect origin direction t1 t2 =
  let c = unitCube ()
  let r = ray origin direction
  let xs = localIntersect r c
  let (localT1, _) = xs.[0]
  let (localT2, _) = xs.[1]
  Expect.equal (List.length xs) 2 ""
  Expect.equal localT1 t1 ""
  Expect.equal localT2 t2 ""

let testShape t = {
  transform = t
  material = Material.defaultMaterial ()
  shape = TestShape
}

[<Tests>]
let tests =
  testList "Tests for Shape" [
    testCase "A ray intersects a sphere at two points" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 4. ""
      Expect.equal xs.[1].t 6. ""

    testCase "A ray intersects a sphere at a tangent" <| fun _ ->
      let r = ray (point 0. 1. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 5. ""
      Expect.equal xs.[1].t 5. ""

    testCase "A ray misses a sphere" <| fun _ ->
      let r = ray (point 0. 2. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 0 ""

    testCase "A ray originates inside a sphere" <| fun _ ->
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -1. ""
      Expect.equal xs.[1].t 1. ""

    testCase "A sphere is behind a ray" <| fun _ ->
      let r = ray (point 0. 0. 5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t -6. ""
      Expect.equal xs.[1].t -4. ""

    testCase "Intersect sets the object on the intersection" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = unitSphere ()
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal (xs.[0].object) s ""
      Expect.equal (xs.[1].object) s ""

    testCase "A Sphere's default transformation" <| fun _ ->
      let s = unitSphere ()
      Expect.equal s.transform (identity ()) ""

    testCase "Changing a sphere's transformation" <| fun _ ->
      let t = translation 2. 3. 4.
      let s = sphere t (defaultMaterial ())
      Expect.equal s.transform t ""

    testCase "Intersecting a scaled sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere (scaling 2. 2. 2.) (defaultMaterial ())
      let xs = intersect r s
      Expect.equal (List.length xs) 2 ""
      Expect.equal xs.[0].t 3. ""
      Expect.equal xs.[1].t 7. ""

    testCase "Intersecting a translated sphere with a ray" <| fun _ ->
      let r = ray (point 0. 0. -5.) (vector 0. 0. 1.)
      let s = sphere (translation 5. 0. 0.) (defaultMaterial ())
      let xs = intersect r s
      Expect.equal (List.length xs) 0 ""

    testCase "The normal on a sphere at a point on the x axis" <| fun _ ->
      let s = unitSphere ()
      let n = normalAt (point 1. 0. 0.) s
      Expect.equal n (vector 1. 0. 0.) ""

    testCase "The normal on a sphere at a point on the y axis" <| fun _ ->
      let s = unitSphere ()
      let n = normalAt (point 0. 1. 0.) s
      Expect.equal n (vector 0. 1. 0.) ""

    testCase "The normal on a sphere at a point on the z axis" <| fun _ ->
      let s = unitSphere ()
      let n = normalAt (point 0. 0. 1.) s
      Expect.equal n (vector 0. 0. 1.) ""

    testCase "The normal on a sphere at a nonaxial point" <| fun _ ->
      let s = unitSphere ()
      let a = (sqrt 3.) / 3.
      let n = normalAt (point a a a) s
      Expect.equal n (vector a a a) ""

    testCase "The normal is a normalized vector" <| fun _ ->
      let s = unitSphere ()
      let a = (sqrt 3.) / 3.
      let n = normalAt (point a a a) s
      Expect.equal n (normalize n) ""

    testCase "Computing the normal on a translated sphere" <| fun _ ->
      let s = sphere (translation 0. 1. 0.) (defaultMaterial ())
      let n = normalAt (point 0. 1.70711 -0.70711) s
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed sphere" <| fun _ ->
      let m = (scaling 1. 0.5 1.) * (rotationZ (Math.PI / 5.))
      let s = sphere m (defaultMaterial ())
      let a = (sqrt 2.) / 2.
      let n = normalAt (point 0. a -a) s
      Expect.equal n (vector 0. 0.97014 -0.24254) ""

    testCase "A sphere has a default material" <| fun _ ->
      let s = unitSphere ()
      let m =
        match s.material with
        | Phong m -> Some m | _ -> None
      Expect.equal m (Some <| defaultMaterialP ()) ""

    testCase "A sphere may be assigned a material" <| fun _ ->
      let s = unitSphere ()
      let m = defaultMaterial ()
      let s2 = { s with material = m }
      let m2 =
        match s2.material with
        | Phong m -> Some m | _ -> None
      Expect.equal m2 (Some <| defaultMaterialP () ) ""

    testCase "Computing the normal on a translated shape" <| fun _ ->
      let s = testShape (translation 0. 1. 0.)
      let n = normalAt (point 0. 1.70711 -0.70711) s
      Expect.equal n (vector 0. 0.70711 -0.70711) ""

    testCase "Computing the normal on a transformed shape" <| fun _ ->
      let s =
        testShape
        <| chain [scale 1. 0.5 1.; rotateZ (Math.PI / 5.)]
      let a = (sqrt 2.) / 2.
      let n = normalAt (point 0. a -a) s
      Expect.equal n (vector 0. 0.97014 -0.24254) ""

    testCase "The normal of a plane is constant everywhere" <| fun _ ->
      let p = defaultPlane ()
      let n1 = localNormal (point 0. 0. 0.) p
      let n2 = localNormal (point 10. 0. -10.) p
      let n3 = localNormal (point -5. 0. 150.) p
      Expect.equal n1 (vector 0. 1. 0.) ""
      Expect.equal n2 (vector 0. 1. 0.) ""
      Expect.equal n3 (vector 0. 1. 0.) ""

    testCase "Intersect with a ray parallel to the plane" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 10. 0.) (vector 0. 0. 1.)
      let xs = localIntersect r p
      Expect.isEmpty xs ""

    testCase "Intersect with a coplanar ray" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 0. 0.) (vector 0. 0. 1.)
      let xs = localIntersect r p
      Expect.isEmpty xs ""

    testCase "A ray intersecting a plane from above" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. 1. 0.) (vector 0. -1. 0.)
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      let (t, object) = xs.[0]
      Expect.equal t 1. ""
      Expect.equal object p ""

    testCase "A ray intersecting a plane from below" <| fun _ ->
      let p = defaultPlane ()
      let r = ray (point 0. -1. 0.) (vector 0. 1. 0.)
      let xs = localIntersect r p
      Expect.equal (List.length xs) 1 ""
      let (t, object) = xs.[0]
      Expect.equal t 1. ""
      Expect.equal object p ""

    testCase "Cube intersection +x" <| fun _ ->
      testCubeIntersect (point 5. 0.5 0.) (vector -1. 0. 0.) 4. 6.

    testCase "Cube intersection -x" <| fun _ ->
      testCubeIntersect (point -5. 0.5 0.) (vector 1. 0. 0.) 4. 6.

    testCase "Cube intersection +y" <| fun _ ->
      testCubeIntersect (point 0.5 5. 0.) (vector 0. -1. 0.) 4. 6.

    testCase "Cube intersection -y" <| fun _ ->
      testCubeIntersect (point 0.5 -5. 0.) (vector 0. 1. 0.) 4. 6.

    testCase "Cube intersection +z" <| fun _ ->
      testCubeIntersect (point 0.5 0. 5.) (vector 0. 0. -1.) 4. 6.

    testCase "Cube intersection -z" <| fun _ ->
      testCubeIntersect (point 0.5 0. -5.) (vector 0. 0. 1.) 4. 6.

    testCase "Cube intersection inside" <| fun _ ->
      testCubeIntersect (point 0. 0.5 0.) (vector 0. 0. 1.) -1. 1.

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
        let n = localNormal point c
        Expect.equal n normal txt

      test (point 1. 0.5 -0.8) (vector 1. 0. 0.) "First"
      test (point -1. -0.2 0.9) (vector -1. 0. 0.) "Second"
      test (point -0.4 1. -0.1) (vector 0. 1. 0.) "Third"
      test (point 0.3 -1. -0.7) (vector 0. -1. 0.) "Fourth"
      test (point -0.6 0.3 1.) (vector 0. 0. 1.) "Fifth"
      test (point 0.4 0.4 -1.) (vector 0. 0. -1.) "Sixth"
      test (point 1. 1. 1.) (vector 1. 0. 0.) "Seventh"
      test (point -1. -1. -1.) (vector -1. 0. 0.) "Eight"

    testCase "A ray misses a cylinder" <| fun _ ->
      let test origin direction txt =
        let cyl = defaultCylinder ()
        let direction = normalize direction
        let r = ray origin direction
        let xs = localIntersect r cyl
        Expect.equal (List.length xs) 0 txt

      test (point 1. 0. 0.) (vector 0. 1. 0.) "First"
      test (point 0. 0. 0.) (vector 0. 1. 0.) "Second"
      test (point 0. 0. -5.) (vector 1. 1. 1.) "Third"

    testCase "A ray strikes a cylinder" <| fun _ ->
      let test origin direction t0 t1 =
        let t = scaling 1. 1000. 1.
        let cyl = cylinder t <| defaultMaterial ()
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
        let cyl = defaultCylinder ()
        let n = localNormal point cyl
        Expect.equal n normal ""

      test (point 1. 0. 0.) (vector 1. 0. 0.)
      test (point 0. 5. -1.) (vector 0. 0. -1.)
      test (point 0. -2. 1.) (vector 0. 0. 1.)
      test (point -1. 1. 0.) (vector -1. 0. 0.)

    testCase "Intersecting a constrained cylinder" <| fun _ ->
      let test point direction count txt =
        let t = chain [
          translateY 1.5
          uniformScale 0.5
        ]
        let cyl = cylinder t <| defaultMaterial ()
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
  ]
