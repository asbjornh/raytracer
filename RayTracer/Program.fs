module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  Stripes.run ()
  0
