module Intersection

type Intersection<'a> = { t: float; object: 'a }

let intersection t o = { t=t; object=o }
