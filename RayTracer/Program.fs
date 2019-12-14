module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  Gradients.run ()
  0
