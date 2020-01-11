module ExperimentalRender

open Color
open Intersection
open Ray
open World

// NOTE: Renders a reflection where the color is determined by the distance from the surface to the reflected object
let distanceReflectionAt world r _ =
  let is = intersect r world
  match (is |> hit) with
  | Some i ->
    let comps = prepareComputations is i r
    let r2 = ray comps.overPoint comps.reflectV
    let is2 = intersect r2 world
    match (is2 |> hit) with
    | Some i -> Color.scale (1. / float i.t) white
    | None -> world.background
  | None -> world.background

let coloredNormals (first, second, third, bg) world r remaining =
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
      add x y |> add z
    | None -> bg
  | None -> bg
