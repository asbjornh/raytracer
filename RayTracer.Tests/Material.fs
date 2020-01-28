module MaterialTest

open Expecto

open Color
open Light
open Matrix
open Material
open Pattern
open Tuple

[<Tests>]

let tests =
  testList "Tests for Material" [
    testCase "Lighting with the eye between the light and the surface" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let eyev = vector 0. 0. -1.
      let normalv = vector 0. 0. -1.
      let light = pointLight (point 0. 0. -10.) (color 1. 1. 1.)
      let result = lighting light position eyev normalv m 0.
      Expect.equal result (color 1.9 1.9 1.9) ""

    testCase "Lighting with the eye between light and surface, eye offset 45°" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let a = (sqrt 2.) / 2.
      let eyev = vector 0. a -a
      let normalv = vector 0. 0. -1.
      let light = pointLight (point 0. 0. -10.) (color 1. 1. 1.)
      let result = lighting light position eyev normalv m 0.
      Expect.equal result  (color 1.0 1.0 1.0) ""

    testCase "Lighting with eye opposite surface, light offset 45°" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let eyev = vector 0. 0. -1.
      let normalv = vector 0. 0. -1.
      let light = pointLight (point 0. 10. -10.) white
      let result = lighting light position eyev normalv m 0.
      Expect.equal result  (color 0.7364 0.7364 0.7364) ""

    testCase "Lighting with eye in the path of the reflection vector" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let a = (sqrt 2.) / 2.
      let eyev = vector 0. -a -a
      let normalv = vector 0. 0. -1.
      let light = pointLight (point 0. 10. -10.) white
      let result = lighting light position eyev normalv m 0.
      Expect.equal result  (color 1.6364 1.6364 1.6364) ""

    testCase "Lighting with the light behind the surface" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let eyev = vector 0. 0. -1.
      let normalv = vector 0. 0. -1.
      let light = pointLight (point 0. 0. 10.) white
      let result = lighting light position eyev normalv m 0.
      Expect.equal result  (color 0.1 0.1 0.1) ""

    testCase "Lighting with the surface in shadow" <| fun _ ->
      let m = defaultMaterialP
      let position = point 0. 0. 0.
      let eyeV = vector 0. 0. -1.
      let normalV = vector 0. 0. -1.
      let light = pointLight (point 0. 0. -10.) white
      let result = lighting light position eyeV normalV m 1.
      Expect.equal result (color 0.1 0.1 0.1) ""

    testCase "Lighting with a pattern applied" <| fun _ ->
      let m = Pattern {
        a = materialC white
        b = materialC black
        pattern = Stripes
        transform = identity
      }
      let m1 =
        match m with
        | Pattern p -> patternAt p.a p.b p.pattern (point 0.9 0. 0.)
        | _ -> failwith "Not a pattern"
      let m2 =
        match m with
        | Pattern p -> patternAt p.a p.b p.pattern (point 1.1 0. 0.)
        | _ -> failwith "Not a pattern"
      Expect.equal m1 (materialC white) "First"
      Expect.equal m2 (materialC black) "Second"
  ]
