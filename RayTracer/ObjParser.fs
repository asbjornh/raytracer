module rec ObjParser

open System.Numerics

open FParsec

open Matrix
open Shape
open Tuple
open Util

let importObj path t mat =
  let result =
    readFile path |> Array.toList |> parse mat
  Shape.group result.objects t mat

let mapHead fn =
  function
  | head :: rest -> (fn head) :: rest
  | l -> l

let addChild g c =
  g |> mapHead (fun (name, els) -> (name, c :: els))

let i = identity

type ParseResult = {
  vertices: Vector4 list
  normals: Vector4 list
  objects: Shape list
}
let parse mat (t: string list) =
  let mutable objects = [("DefaultObject", [])]
  let mutable groups = [("DefaultGroup", [])]
  let mutable vertices = []
  let mutable normals = []

  t
  |> List.collect (fun s ->
    s.Trim (' ')
    |> parseLine
    |> Option.toList
  )
  |> List.iter (
    function
    | Vertex (x, y, z) ->
      vertices <- (point x y z) :: vertices

    | Normal (x, y, z) ->
      normals <- (vector x y z) :: normals

    | Face f ->
      let g = getFaces mat (List.rev vertices) f
      groups <- addChild groups g

    | FaceNormal f ->
      let g = getSmoothFaces mat (List.rev vertices) (List.rev normals) f
      groups <- addChild groups g
    
    | FaceTex f ->
      let g = getFaces mat (List.rev vertices) (List.map fst f)
      groups <- addChild groups g

    | FaceTexNormal f ->
      let normalIds = f |> List.map (fun (v, t, n) -> (v, n))
      let g = getSmoothFaces mat (List.rev vertices) (List.rev normals) normalIds
      groups <- addChild groups g

    | Group name ->
      groups <- (name, []) :: groups

    | Object name ->
      objects <- objects |> mapHead (fun (name, els) ->
        (name, groups @ els) // Add entire group stack to current object
      )
      objects <- (name, []) :: objects // Add the new object
      groups <- [("DefaultGroup", [])] // Empty group stack
  )

  // If there are any unused groups remaining, add to last object
  groups
  |> List.filter (snd >> List.isEmpty >> (not))
  |> List.iter (fun g ->
    objects <- addChild objects g
  )

  parseResult vertices normals objects

let parseResult vertices normals objects =
  let os =
    objects
    |> List.map (fun (n, els) ->
      els
      |> List.filter (snd >> List.isEmpty >> (not))
      |> List.map (fun (n, els) -> namedGroupT n (List.rev els) i)
      |> namedGroupT n <| i
    )
    |> List.filter (fun s -> not (List.isEmpty s.children))
    |> List.rev

  {
    vertices = List.rev vertices
    normals = List.rev normals
    objects = os
  }

let getFaces mat vertices =
  List.map (int >> flip (-) 1) >> polys mat vertices

let getSmoothFaces mat vertices normals =
  List.map (fun (v, n) ->
    (int v - 1, int n - 1)
  ) >> smoothPolys mat vertices normals

type LineResult =
  | Vertex of (float * float * float)
  | Normal of (float * float * float)
  | Face of int64 list
  | FaceNormal of (int64 * int64) list
  | FaceTex of (int64 * int64) list
  | FaceTexNormal of (int64 * int64 * int64) list
  | Group of string
  | Object of string

let parseOne parser typ str =
  match run parser str with
  | Success (res, _, _) -> Some <| typ res
  | Failure _ -> None

let parseLine str =
  [ parseOne vertex Vertex
    parseOne normal Normal
    parseOne faceTexNormal FaceTexNormal
    parseOne faceNormal FaceNormal
    parseOne faceTex FaceTex
    parseOne face Face
    parseOne group Group
    parseOne obj Object ]
  |> List.tryPick (fun parser -> parser str)

let str = pstring
let space = pchar ' '
let coord = pfloat .>> (opt space)
let vertexCoords = tuple3 coord coord coord
let vertex = pchar 'v' >>. spaces >>. vertexCoords
let normal = str "vn" >>. spaces >>. vertexCoords
let group = pchar 'g' >>. spaces >>. restOfLine false
let obj = pchar 'o' >>. spaces >>. restOfLine false



let faceBase coord = pchar 'f' >>. spaces >>. (sepBy coord space)

let face = faceBase pint64
let faceNormal =
  faceBase (tuple2 pint64 (str "//" >>. pint64))
let faceTex =
  faceBase (tuple2 pint64 (str "/" >>. pint64))
let faceTexNormal =
  faceBase (tuple3 pint64 (str "/" >>. pint64) (str "/" >>. pint64))
