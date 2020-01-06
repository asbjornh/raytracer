module Intersection

open System.Numerics

open Material
open Matrix
open Ray
open Shape
open Tuple
open Transform
open Util

type Intersection = {
  t: float32;
  object: Shape
  triangleUV: (float32 * float32) option
}

let getT i = i.t

let intersection t o = { t=t; object=o; triangleUV=None }
let intersectionUV t o uv = { t=t; object=o; triangleUV=uv}

let intersections (l: Intersection list) =
  List.sortBy getT l

let visible i = i.t >= 0.f
let hit (l: Intersection list) =
  match List.filter visible l with
  | [] -> None
  | visibleShapes -> Some (List.minBy getT visibleShapes)

let pickIndex (a: float32) b =
  if (a = b) then a
  else if (a = 1.f) then b
  else a

let rec indexForMaterial = function
  | Transparent m -> m.index
  | Mix m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Blend m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Fresnel m ->
    pickIndex (indexForMaterial m.a) (indexForMaterial m.b)
  | Gradient _ | Pattern _ | Phong _ | NormalMap _ | Luminance _
  | Reflective _ | TestPattern _ | Textured _ | InvisFloor _ -> 1.f

let indexFromIntersection (s: Shape) =
  indexForMaterial s.material

let refractiveIndexes (is: Intersection list) (hit: Intersection) =
  is |> List.fold (fun (containers, n1, n2) i ->
    let newN1 =
      if not (refEq i hit) then n1
      else if List.isEmpty containers then 1.f
      else indexFromIntersection (List.last containers)

    let newC =
      if (containsRef i.object containers) then
        List.filter (fun el -> not (refEq el i.object)) containers
      else List.append containers [i.object]

    let newN2 =
      if not (refEq i hit) then n2
      else if List.isEmpty newC then 1.f
      else indexFromIntersection (List.last newC)
    (newC, newN1, newN2)
  ) ([], 1.f, 1.f)
  |> (fun (_, n1, n2) -> (n1, n2))


// Bias for fixing shadow and pattern acne
let bias origin point objectT normal =
  // NOTE: Scaling epsilon by the distance to origin fixes some shadow acne
  let d = Vector4.Distance (origin, point)
  let (_, scale, _, _) = Matrix4x4.Decompose objectT
  let minScale = List.min [scale.X; scale.Y; scale.Z]
  let t = uniformScale (1.f / min 1.f minScale)
  // NOTE: Scaling the normal by the inverse of the
  let newNormal = transform t normal
  d * epsilon32 * newNormal

let rec resolveMaterial s =
  match s.parent with
  | Some p -> resolveMaterial p
  | None -> s.material

type Computation = {
  t: float32
  object: Shape
  point: Vector4
  eyeV: Vector4
  normalV: Vector4
  reflectV: Vector4
  inside: bool
  overPoint: Vector4
  underPoint: Vector4
  n1: float32
  n2: float32
}

let prepareComputations (is: Intersection list) (hit: Intersection) r =
  let point = position hit.t r
  let normalV =
    match hit.triangleUV with
    | Some uv -> normalAtUV hit.object point uv
    | None -> normalAt hit.object point
  let eyeV = negate r.direction
  let inside = (dot normalV eyeV) < 0.f
  let normalV =
    (if inside then negate else id) normalV
  let reflectV = reflect normalV r.direction
  let (n1, n2) = refractiveIndexes is hit
  let b = bias r.origin point hit.object.transform normalV
  {
    t = hit.t
    object = { hit.object with material = resolveMaterial hit.object }
    point = point
    eyeV = eyeV
    normalV = normalV
    reflectV = reflectV
    inside = inside
    overPoint = point + b
    underPoint = point - b
    n1 = n1
    n2 = n2
  }

let intersect (ray: Ray) (s: Shape) =
  s |> shapeIntersect ray |> List.map (fun (t, o, uv) -> intersectionUV t o uv)
