module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  PlaneScene.run ()
  0