module CameraTest

open System
open Expecto

open Camera
open Color
open Matrix
open Transform
open Tuple
open Util
open World

let testCam hSize vSize fov = cameraT hSize vSize fov identity
let ExpectEqual actual expected =
  Expect.isTrue
  <| looseEq32 actual expected
  <| Expect.defaultDiffPrinter expected actual

[<Tests>]
let tests =
  testList "Tests for Camera" [
    testCase "Constructing a camera" <| fun _ ->
      let hsize = 160
      let vsize = 120
      let fov = MathF.PI / 2.f
      let c = testCam hsize vsize fov
      Expect.equal c.hSize 160 ""
      Expect.equal c.vSize 120 ""
      Expect.equal c.fov (MathF.PI / 2.f) ""
      Expect.equal c.transform identity ""

    testCase "The pixel size for a horizontal canvas" <| fun _ ->
      let c = testCam 200 125 (MathF.PI / 2.f)
      ExpectEqual c.pixelSize 0.01f

    testCase "The pixel size for a vertical canvas" <| fun _ ->
      let c = testCam 125 200 (MathF.PI / 2.f)
      ExpectEqual c.pixelSize 0.01f

    testCase "Constructing a ray through the center of the canvas" <| fun _ ->
      let c = testCam 201 101 (MathF.PI / 2.f)
      let r = rayForPixel 100 50 c
      Expect.equal r.origin (point 0. 0. 0.) ""
      Expect.equal r.direction (vector 0. 0. -1.) ""
      
    testCase "Constructing a ray through a corner of the canvas" <| fun _ ->
      let c = testCam 201 101 (MathF.PI / 2.f)
      let r = rayForPixel 0 0 c
      Expect.equal r.origin (point 0. 0. 0.) ""
      Expect.equal r.direction (vector 0.66519 0.33259 -0.66851) ""

    testCase "Constructing a ray when the camera is transformed" <| fun _ ->
      let c =
        cameraT 201 101 (MathF.PI / 2.f)
        <| (translate 0.f -2.f 5.f) * (rotateY (MathF.PI / 4.f))
      let r = rayForPixel 100 50 c
      let a = (sqrt 2.) / 2.
      Expect.equal r.origin (point 0. 2. -5.) ""
      Expect.equal r.direction (vector a 0. -a) ""

    testCase "Rendering a world with a camera" <| fun _ ->
      let w = defaultWorld ()
      let pos = point 0. 0. -5.
      let target = point 0. 0. 0.
      let c = camera 11 11 (MathF.PI / 2.f) pos target 
      let image = render c w
      Expect.equal (image.[5].[5]) (color 0.38066 0.47583 0.2855) ""
  ]
