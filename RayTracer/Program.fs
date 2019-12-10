module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  AnimatedSphere.run ()
  0