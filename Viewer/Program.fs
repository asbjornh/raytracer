open System
open System.IO

open Suave
open Suave.Files
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Logging

open Serialize

let wsResponse =
  serialize
  >> System.Text.Encoding.ASCII.GetBytes
  >> ByteSegment

let ws (webSocket: WebSocket) context =
  socket {
    let mutable loop = true
    while loop do
      let! msg = webSocket.read ()

      match msg with
      | (Text, data, true) ->
        let str = UTF8.toString data
        printfn "Message %A" str
        let response =
          wsResponse [("h", Int 4); ("w", Int 8)]
        do! webSocket.send Text response true

      | (Close, _, _) ->
        printfn "Close"
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true
        loop <- false

      | _ -> ()
  }

let app =
  choose
    [ path "/connect" >=> handShake ws
      GET >=>  choose [ path "/" >=> file "./public/index.html" ; browseHome ]
      RequestErrors.NOT_FOUND "Page not found" ]


let config = {
  defaultConfig with
    homeFolder = Some (Path.GetFullPath "./public")
    logger = Targets.create Verbose [||]
}

[<EntryPoint>]
let main argv =
  startWebServer config (app)
  0
