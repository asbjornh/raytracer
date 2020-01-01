module ObjParserTest

open Expecto

open Matrix
open ObjParser
open Shape
open Tuple

let getTriangle s =
  match s.shape with
  | Triangle p -> p | _ -> failwith "Not a Poly"

let getGroup s =
  match s.shape with
  | Group g -> g | _ -> failwith "Not a Group"

[<Tests>]
let tests =
  testList "Tests for ObjParser" [
    testCase "Ignoring unrecognized lines" <| fun _ ->
      let gibberish = [
        "There was a young lady named Bright​"
        "who traveled much faster than light.​"
        "She set out one day​"
        "in a relative way,​"
        "and came back the previous night.​"
      ]
      let r = parse gibberish
      Expect.equal r.ignoredLines 5 ""

    testCase "Vertex records" <| fun _ ->
      let file = [
        "v -1 1 0"
        "v -1.0000 0.5000 0.0000"
        "v 1 0 0"
        "v 1 1 0"
      ]
      let r = parse file
      Expect.equal (List.length r.vertices) 4 ""
      Expect.equal r.vertices.[0] (point -1. 1. 0.) ""
      Expect.equal r.vertices.[1] (point -1. 0.5 0.) ""
      Expect.equal r.vertices.[2] (point 1. 0. 0.) ""
      Expect.equal r.vertices.[3] (point 1. 1. 0.) ""

    testCase "Parsing triangle faces" <| fun _ ->
      let file = [
        "v -1 1 0"
        "v -1 0 0"
        "v 1 0 0"
        "v 1 1 0"
        ""
        "f 1 2 3"
        "f 1 3 4"
      ]
      let r = parse file
      let g = r.defaultGroup
      Expect.equal (List.length g.children) 2 ""
      let t1 = getTriangle g.children.[0]
      let t2 = getTriangle g.children.[1]
      Expect.equal (List.length r.vertices) 4 ""
      Expect.equal t1.p1 r.vertices.[0] ""
      Expect.equal t1.p2 r.vertices.[1] ""
      Expect.equal t1.p3 r.vertices.[2] ""
      Expect.equal t2.p1 r.vertices.[0] ""
      Expect.equal t2.p2 r.vertices.[2] ""
      Expect.equal t2.p3 r.vertices.[3] ""

    testCase "Parsing triangle faces with texture indices and weird whitespace" <| fun _ ->
      let file = [
        "v  -1 1 0"
        "v  -1 0 0"
        "v  1 0 0"
        "v  1 1 0"
        "# Some comment"
        ""
        "f 1/1 2/1 3/1 " // NOTE: vertex index/texture index
        "f 1/1 3/1 4/1 "
        "g BlahBlah01" // NOTE: Should support numbers in group
        "f 1/1 3/1 4/1 "
      ]
      let r = parse file
      let g = r.defaultGroup
      Expect.equal (List.length g.children) 2 ""
      let t1 = getTriangle g.children.[0]
      let t2 = getTriangle g.children.[1]
      Expect.equal (List.length r.vertices) 4 ""
      Expect.equal (List.length r.groups) 1 ""
      Expect.equal t1.p1 r.vertices.[0] ""
      Expect.equal t1.p2 r.vertices.[1] ""
      Expect.equal t1.p3 r.vertices.[2] ""
      Expect.equal t2.p1 r.vertices.[0] ""
      Expect.equal t2.p2 r.vertices.[2] ""
      Expect.equal t2.p3 r.vertices.[3] ""

    testCase "Triangulating polygons" <| fun _ ->
      let file = [
        "v -1 1 0"
        "v -1 0 0"
        "v 1 0 0"
        "v 1 1 0"
        "v 0 2 0"
        ""
        "f 1 2 3 4 5"
      ]
      let r = parse file
      let g = r.defaultGroup.children.[0]

      Expect.equal (List.length g.children) 3 ""
      let t1 = getTriangle g.children.[0]
      let t2 = getTriangle g.children.[1]
      let t3 = getTriangle g.children.[2]

      Expect.equal (List.length r.vertices) 5 ""
      Expect.equal t1.p1 r.vertices.[0] ""
      Expect.equal t1.p2 r.vertices.[1] ""
      Expect.equal t1.p3 r.vertices.[2] ""
      Expect.equal t2.p1 r.vertices.[0] ""
      Expect.equal t2.p2 r.vertices.[2] ""
      Expect.equal t2.p3 r.vertices.[3] ""
      Expect.equal t3.p1 r.vertices.[0] ""
      Expect.equal t3.p2 r.vertices.[3] ""
      Expect.equal t3.p3 r.vertices.[4] ""

    testCase "Triangles in groups" <| fun _ ->
      let file = [
        "v -1 1 0"
        "v -1 0 0"
        "v 1 0 0"
        "v 1 1 0"
        ""
        "g FirstGroup"
        "f 1 2 3"
        "g SecondGroup"
        "f 1 3 4"
      ]
      let r = parse file

      Expect.equal (List.length r.groups) 2 ""
      let g1 =
        r.groups
        |> List.find (fun s -> (getGroup s).name = "FirstGroup")
      let g2 =
        r.groups
        |> List.find (fun s -> (getGroup s).name = "SecondGroup")
      let t1 = g1.children.[0] |> getTriangle
      let t2 = g2.children.[0] |> getTriangle

      Expect.equal (List.length r.vertices) 4 ""
      Expect.equal t1.p1 r.vertices.[0] ""
      Expect.equal t1.p2 r.vertices.[1] ""
      Expect.equal t1.p3 r.vertices.[2] ""
      Expect.equal t2.p1 r.vertices.[0] ""
      Expect.equal t2.p2 r.vertices.[2] ""
      Expect.equal t2.p3 r.vertices.[3] ""

    testCase "Converting an OBJ file to a group" <| fun _ ->
      let g = objFromFile "./fixtures/triangles.obj" <| identity ()
      Expect.equal (List.length g.children) 2 ""
      Expect.equal ((g.children.[0] |> getGroup).name) "FirstGroup" ""
      Expect.equal ((g.children.[1] |> getGroup).name) "SecondGroup" ""
  ]
