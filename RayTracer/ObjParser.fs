module rec ObjParser

open FParsec

open Tuple

type ParseResult = {
  ignoredLines: int
  vertices: Tuple list
}

let exists = function | Some _ -> true | None -> false
let parse (txt: string list) =
  let vertices =
    List.map parseVertex txt
    |> List.collect Option.toList

  {
    ignoredLines = List.length txt - List.length vertices
    vertices = vertices
  }

let str s = pstring s
let coords = sepBy pfloat <| pstring " "
let vertex =
  pchar 'v' >>. str " " >>. coords

let parseVertex s =
  match run vertex s with
  | Success (s, _, _) ->
    match s with
    | [x; y; z] -> Some <| point x y z
    | _ -> None
  | _ -> None
