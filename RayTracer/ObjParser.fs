module rec ObjParser

open FParsec

open Matrix
open Shape
open Tuple
open Util

let objFromFile path t =
  let result =
    readFile path |> Array.toList |> parse
  Shape.group result.objects t

let mapHead fn =
  function
  | head :: rest -> (fn head) :: rest
  | l -> l

let i = identity

type ParseResult = {
  vertices: Tuple list
  objects: Shape list
}
let parse (t: string list) =
  let mutable objects = [("DefaultObject", [])]
  let mutable groups = [("DefaultGroup", [])]
  let mutable vertices = []

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

    | Face f ->
      let g = getFaces (List.rev vertices) f
      groups <- groups |> mapHead (fun (name, els) ->
        (name, g :: els)
      )

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
    objects <- objects |> mapHead (fun (name, els) ->
      (name, g :: els)
    )
  )

  parseResult vertices objects

let parseResult vertices objects =
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
    objects = os
  }

let getFaces vertices =
  List.map (List.head >> int >> flip (-) 1) >> polys vertices

type LineResult =
  | Vertex of (float * float * float)
  | Face of int64 list list
  | Group of string
  | Object of string

let parseOne parser typ str =
  match run parser str with
  | Success (res, _, _) -> Some <| typ res
  | Failure _ -> None

let parseLine str =
  [ parseOne vertex Vertex
    parseOne face Face
    parseOne group Group
    parseOne obj Object ]
  |> List.tryPick (fun parser -> parser str)

let str = pstring
let coord = pfloat .>> (opt <| str " ")
let vertexCoords = tuple3 coord coord coord
let vertex = pchar 'v' >>. spaces >>. vertexCoords
let faceCoords = sepBy (sepBy1 pint64 <| str "/") (str " ")
let face = pchar 'f' >>. spaces >>. faceCoords
let group = pchar 'g' >>. spaces >>. restOfLine false
let obj = pchar 'o' >>. spaces >>. restOfLine false
