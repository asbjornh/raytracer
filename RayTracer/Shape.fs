module rec Shape

open Matrix
open Material
open Tuple
open Util

type ShapeType =
  | Sphere
  | Plane
  | Cube
  | Cylinder
  | OpenCylinder
  | Cone
  | DoubleCone
  | TestShape
  | Group of Group

type Shape = {
  mutable transform: Matrix
  mutable material: Material
  shape: ShapeType
  mutable parent: Shape option
  mutable children: Shape list
}

type Group = {
  bounds: (float * float) * (float * float) * (float * float)
}

let localIntersect ray (s: Shape) =
  match s.shape with
  | Sphere -> Sphere.intersect ray s
  | Plane -> Plane.intersect ray s
  | Cylinder -> Cylinder.intersect ray s
  | OpenCylinder -> Cylinder.intersectOpen ray s
  | Cube -> Cube.intersect ray s
  | Cone -> Cone.intersect -1. 0. ray s
  | DoubleCone -> Cone.intersect -1. 1. ray s
  | TestShape -> [(0., s)]
  | Group g ->
    let (boundsX, boundsY, boundsZ) = g.bounds
    let i = Cube.intersectBox boundsX boundsY boundsZ ray s
    if List.isEmpty i then []
    else
      List.collect (shapeIntersect ray) s.children

let shapeIntersect ray (shape: Shape) =
  localIntersect
  <| Ray.transform (inverse shape.transform) ray
  <| shape

let uvAt p s =
  match s.shape with
  | Sphere -> Sphere.uv p
  | Plane -> Plane.uv p
  | TestShape -> (0., 0.)
  | Cylinder -> failwith "Missing UV implementation for Plane"
  | OpenCylinder -> failwith "Missing UV implementation for OpenCylinder"
  | Cone -> failwith "Missing UV implementation for Cone"
  | DoubleCone -> failwith "Missing UV implementation for DoubleCone"
  | Cube -> failwith "Missing UV implementation for Cube"
  | Group _ -> failwith "Missing UV implementation for Group"

let localNormal (s: Shape) p =
  match s.shape with
  | Sphere -> p - (point 0. 0. 0.)
  | Plane -> vector 0. 1. 0.
  | TestShape -> p
  | Cylinder -> Cylinder.normal p
  | OpenCylinder -> vector p.X 0. p.Z
  | Cone -> Cone.normal -1. 0. p
  | DoubleCone -> Cone.normal -1. 1. p
  | Cube -> Cube.normal p
  | Group _ -> failwith "Missing localNormal implementation for Group"

let normalAt shape =
  worldToObject shape
  >> localNormal shape
  >> normalToWorld shape

let worldToObject (shape: Shape) (p: Tuple) =
  match shape.parent with
  | Some parent -> worldToObject parent p
  | None -> p
  |> multiplyT (inverse shape.transform)

let normalToWorld (shape: Shape) (v: Tuple) =
  let t = shape.transform |> inverse |> transpose
  let n1 = multiplyT t v |> toVector |> normalize
  match shape.parent with
  | Some parent -> normalToWorld parent n1
  | None -> n1


let bounds (minX, maxX) (minY, maxY) (minZ, maxZ) =
  [
    point minX maxY maxZ; point minX maxY minZ;
    point maxX maxY maxZ; point maxX maxY minZ;
    point minX minY maxZ; point minX minY minZ;
    point maxX minY maxZ; point maxX minY minZ;
  ]

let boundsForShape s =
  let cube = bounds (-1., 1.) (-1., 1.) (-1., 1.)
  match s.shape with
  | Sphere -> cube
  | Plane -> bounds (-1., 1.) (0., 0.) (-1., 1.)
  | TestShape -> []
  | Cylinder -> cube
  | OpenCylinder -> cube
  | Cone -> bounds (-1., 1.) (-1., 0.) (-1., 1.)
  | DoubleCone -> cube
  | Cube -> cube
  | Group g ->
    if List.isEmpty s.children then cube
    else boundsForShapes s.children

let boundsForShapes (objects: Shape list) =
  objects
  |> List.collect (fun s ->
    s |> boundsForShape |> List.map (multiplyT s.transform)
  )

let boundingBox (objects: Shape list) =
  if List.isEmpty objects
  then ( (0., 0.), (0., 0.), (0., 0.) )
  else
    let (xs, ys, zs) =
      objects |> boundsForShapes |> List.map toXYZ |> List.unzip3
    ( (List.min xs, List.max xs),
      (List.min ys, List.max ys),
      (List.min zs, List.max zs) )

let shape s t m = {
  transform = t
  material = m
  shape = s
  parent = None
  children = []
}
let shapeM s m = shape s (identity ()) m
let shapeT s t = shape s t <| defaultMaterial ()
let defaultShape s = shape s <| identity () <| defaultMaterial ()
let sphere t = shape Sphere t
let plane t = shape Plane t
let cube t = shape Cube t
let cylinder t = shape Cylinder t
let cone t = shape Cone t
let doubleCone t = shape DoubleCone t
let openCylinder t = shape OpenCylinder t

let updateParent parent shape =
  let newS = { shape with parent = Some parent }
  newS.children |> List.iter (fun s ->
    s.parent <- Some newS
  )
  newS

let group c t m =
  let g = Group { bounds = boundingBox c }
  let s = shape g t m
  s.children <- c |> List.map (updateParent s)
  s

let unitSphere () = defaultShape Sphere
let sphereT t = shapeT Sphere t
let sphereM m = shapeM Sphere m
let defaultPlane () = defaultShape Plane
let unitCube () = defaultShape Cube
let defaultCylinder () = defaultShape Cylinder
let defaultOpenCylinder () = defaultShape OpenCylinder
let defaultCone () = defaultShape Cone
let defaultDoubleCone () = defaultShape DoubleCone
let groupT c t = group c t <| defaultMaterial ()
