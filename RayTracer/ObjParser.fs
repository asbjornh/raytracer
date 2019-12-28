module rec ObjParser

open FParsec

open Matrix
open Shape
open Tuple
open Util

type ParseResult = {
  ignoredLines: int
  vertices: Tuple list
  defaultGroup: Shape
  groups: Shape list
}

let objFromFile path =
  let result =
    readFile path |> Array.toList |> parse
  let children =
    if List.isEmpty result.defaultGroup.children
    then result.groups
    else result.defaultGroup :: result.groups
  groupT children

let parse (t: string list) =
  let txt =
    t |> List.filter (String.length >> flip (>) 0)
    |> String.concat "\n"

  let i = identity ()

  match run obj txt with
  | Success (res, _, _) ->
    let (vs, fs, gs) = res
    let vertices = getVertices vs
    let faces =
      getFaces vertices fs |> groupT <| i
    let groups =
      gs |> List.map (fun (name, fs) ->
        namedGroupT name (getFaces vertices fs) i
      )
    let parsedLines =
      List.length vs +
      List.length fs +
      List.length gs +
      (List.collect snd gs |> List.length)

    {
      ignoredLines = List.length t - parsedLines
      vertices = vertices
      defaultGroup = faces
      groups = groups
    }
  | Failure (e, _, _) -> failwith e

let getVertices =
  List.collect (fun l ->
    match l with
    | [x; y; z] -> [point x y z]
    | _ -> []
  )

let getFaces vertices =
  List.collect
    (List.map (int >> flip (-) 1) >> polys vertices)

let polys vs face =
  match face with
  | [one; two; three] -> 
    [defaultPoly vs.[one] vs.[two] vs.[three]]
  | [] -> []
  | _ ->
    face |> List.map (fun i -> vs.[i])
    |> fanTriangulation

let fanTriangulation = function
  | [] -> []
  | start :: rest ->
    List.pairwise rest
    |> List.map (fun (second, third) ->
      defaultPoly start second third
    )

let str = pstring
let coords = sepBy pfloat (str " ")
let vertex = pchar 'v' >>. str " " >>. coords
let vertices = sepEndBy vertex newline
let face = pchar 'f' >>. str " " >>. coords
let faces = sepEndBy face newline
let groupStart = pchar 'g' >>. str " " >>. (manyChars asciiLetter)
let group = groupStart .>>. (newline >>. faces)
let obj = tuple3 vertices faces (many group)
