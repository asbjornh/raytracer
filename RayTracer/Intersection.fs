module Intersection

type Intersection<'a> = { t: float; object: 'a }

let getT i = i.t

let intersection t o = { t=t; object=o }

let intersections (l: Intersection<'a> list) =
  List.sortBy getT l

let visible i = i.t >= 0.
let hit (l: Intersection<'a> list) =
  let visibleEls = List.filter visible l
  match visibleEls with
  | [] -> None
  | _ -> Some (List.minBy getT visibleEls)
