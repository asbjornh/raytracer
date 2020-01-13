module ExperimentalRender

open Color
open Intersection
open Ray
open World

let coloredNormals (first, second, third, lazer) world r remaining =
  let is = intersect r world
  match (is |> hit) with
  | Some i ->
    let comps = prepareComputations is i r
    let r2 = ray comps.overPoint comps.reflectV
    let is2 = intersect r2 world
    match (is2 |> hit) with
    | Some i ->
      let comps = prepareComputations is i r
      let n = comps.normalV
      let x = scale (float n.X) first
      let y = scale (float n.Y) second
      let z = scale (float -n.Z) third
      let factor = 
        if lazer then 1. / float i.t
        else 1.
      add x y |> add z |> Color.scale factor
    | None -> world.background
  | None -> world.background
