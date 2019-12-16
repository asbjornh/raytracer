module Main

open System
open ShellProgressBar

[<EntryPoint>]
let main argv =
  SphereRing.run ()
  0
