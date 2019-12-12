module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  SphereScene.run ()
  0