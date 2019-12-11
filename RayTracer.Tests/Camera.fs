module CameraTest

open System
open Expecto

open Camera
open Matrix
open Util

let ExpectEqual actual expected =
  Expect.isTrue
  <| looseEq actual expected
  <| Expect.defaultDiffPrinter expected actual

[<Tests>]
let tests =
  testList "Tests for Camera" [
    testCase "Constructing a camera" <| fun _ ->
      let hsize = 160
      let vsize = 120
      let fov = Math.PI / 2.
      let c = camera hsize vsize fov
      Expect.equal c.hSize 160 ""
      Expect.equal c.vSize 120 ""
      Expect.equal c.fov (Math.PI / 2.) ""
      Expect.equal c.transform (identity ()) ""

    testCase "The pixel size for a horizontal canvas" <| fun _ ->
      let c = camera 200 125 (Math.PI / 2.)
      ExpectEqual c.pixelSize 0.01

    testCase "The pixel size for a vertical canvas" <| fun _ ->
      let c = camera 125 200 (Math.PI / 2.)
      ExpectEqual c.pixelSize 0.01
  ]
