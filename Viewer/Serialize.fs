module rec Serialize

type JsonEntries = (string * JsonValue) list

type JsonValue =
  | String of string
  | Float of float
  | Int of int
  | Bool of bool
  | Array of JsonValue list
  | Entries of JsonEntries

let serializeValue =
  function
  | String s -> sprintf "\"%s\"" s
  | Float f -> sprintf "%f" f
  | Int i -> sprintf "%i" i
  | Bool b -> sprintf "%b" b
  | Array a ->
    a |> List.map serializeValue
    |> String.concat ", "
    |> sprintf "[ %s ]"
  | Entries e -> serialize e

let serialize entries =
  entries |> List.map (fun (key, value) ->
    sprintf "\"%s\": %s" key <| serializeValue value
  )
  |> String.concat ", "
  |> sprintf "{ %s }"
