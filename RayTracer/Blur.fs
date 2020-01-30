module Blur

open System
open ShellProgressBar

open Color
open Util

let withProgress len txt fn =
  let bar = new ProgressBar (len, txt, ConsoleColor.Yellow)
  let result = fn (fun _ -> bar.Tick ())
  printfn "\n" // To avoid CLI glitch after rendering
  result

let boxBlur size (image: Color[][]) =
  let len = Array.length image * (Array.length image.[0])
  withProgress len "Blurring" (fun tick ->
    image
    |> map2diParallel (fun x y c ->
      tick ()
      image
      |> subGrid x y size |> Array.concat
      |> flip appendTo c
      |> Array.toList
      |> Color.average
    )
  )

let medianFilter size (image: Color[][]) =
  let medianIndex =
    (float size * float size) / 2. |> Math.Floor |> int
  image
  |> map2diParallel (fun x y c ->
    image
    |> subGrid x y size |> Array.concat
    |> Array.sortBy (fun c ->
      let (r, g, b) = c.Return
      r + g + b
    )
    |> Array.item medianIndex
  )

let weight x y k l intensityXY intensityKL sD sR =
  let left = ((float x - float k) ** 2. + (float y - float l) ** 2.) / (2. * (sD ** 2.))
  let right = ((abs <| intensityXY - intensityKL) ** 2.) / (2. * (sR ** 2.))
  exp (-left - right)

let sumWeights (fn: int -> int -> float -> float) arr =
  map2di fn arr |> Array.concat |> Array.sum

let bilateralFilter size sD sR tick (image: float[][]) =
  image
  |> map2diParallel (fun x y iXY ->
    tick ()
    let samples = image |> subGrid x y size
    let a =
      samples |> sumWeights (fun k l iKL ->
        iKL * (weight x y (x + k) (y + l) iXY iKL sD sR)
      )
    let b =
      samples |> sumWeights (fun k l iKL ->
        weight x y (x + k) (y + l) iXY iKL sD sR
      )
    a / b
  )
