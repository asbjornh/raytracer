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
}

let exists = function | Some _ -> true | None -> false
let parse (txt: string list) =
  let vertices =
    List.map parseVertex txt
    |> List.collect Option.toList
  let faces =
    List.map parseFace txt
    |> List.collect Option.toList
    |> List.collect (List.map (int >> flip (-) 1) >> polys vertices)
    |> groupT <| identity ()

  {
    ignoredLines = List.length txt - List.length vertices
    vertices = vertices
    defaultGroup = faces
  }

let polys vs face =
  match face with
  | [one; two; three] -> 
    [defaultPoly vs.[one] vs.[two] vs.[three]]
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

let str s = pstring s
let coords = sepBy pfloat <| pstring " "
let vertex = pchar 'v' >>. str " " >>. coords
let face = pchar 'f' >>. str " " >>. coords

let parseVertex s =
  match run vertex s with
  | Success ([x; y; z], _, _) -> Some <| point x y z
  | _ -> None

let parseFace s =
  match run face s with
  | Success (s, _, _) ->
    if List.length s >= 3 then Some s else None
  | _ -> None
