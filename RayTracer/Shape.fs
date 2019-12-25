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
}

type Group = {
  children: Shape list
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
    List.collect (shapeIntersect ray) g.children

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

let localNormal p (s: Shape) =
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

let normalAt point (shape: Shape) =
  let invT = inverse shape.transform
  let localP = multiplyT invT point
  let localN = localNormal localP shape
  let worldN = multiplyT (transpose invT) localN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)

let groupParents (group: Group) (inner: Shape) =
  group.children |> List.collect (fun c ->
    match c.shape with
    | Group g -> c :: groupParents g inner
    | _ -> if refEq c inner then [c] else []
  )

let worldToObject (p: Tuple) (shape: Shape) (inner: Shape) =
  match shape.shape with
  | Group g ->
    shape :: groupParents g inner
    |> List.map (fun s -> inverse s.transform)
    |> List.rev
    |> List.reduce multiply
    |> flip multiplyT p
  | _ -> multiplyT (shape.transform) p

let shape s t m = { transform = t; material = m; shape = s }
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
let group c t = shape (Group { children = c }) t

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
