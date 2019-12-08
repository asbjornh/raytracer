module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  SimpleCircle.run ()
  0