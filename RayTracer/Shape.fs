module rec Shape

open Matrix
open Material
open Tuple

type ShapeType =
  | Sphere
  | Plane
  | Cube
  | Cylinder
  | OpenCylinder
  | TestShape

type Shape = {
  mutable transform: Matrix
  mutable material: Material
  shape: ShapeType
}

let localIntersect ray (s: Shape) =
  match s.shape with
  | Sphere -> Sphere.intersect ray s
  | Plane -> Plane.intersect ray s
  | Cylinder -> Cylinder.intersect ray s
  | OpenCylinder -> Cylinder.intersectOpen ray s
  | Cube -> Cube.intersect ray s
  | TestShape -> [(0., s)]

let localNormal p (s: Shape) =
  match s.shape with
  | Sphere -> p - (point 0. 0. 0.)
  | Plane -> vector 0. 1. 0.
  | TestShape -> p
  | Cylinder -> Cylinder.normal p
  | OpenCylinder -> vector p.X 0. p.Z
  | Cube -> Cube.normal p

let shapeIntersect ray (shape: Shape) =
  localIntersect
  <| Ray.transform (inverse shape.transform) ray
  <| shape

let normalAt point (shape: Shape) =
  let invT = inverse shape.transform
  let localP = multiplyT invT point
  let localN = localNormal localP shape
  let worldN = multiplyT (transpose invT) localN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)

let shape s t m = { transform = t; material = m; shape = s }
let shapeM s m = shape s (identity ()) m
let shapeT s t = shape s t <| defaultMaterial ()
let defaultShape s = shape s <| identity () <| defaultMaterial ()
let sphere t = shape Sphere t
let plane t = shape Plane t
let cube t = shape Cube t
let cylinder t = shape Cylinder t
let openCylinder t = shape OpenCylinder t

let unitSphere () = defaultShape Sphere
let sphereT t = shapeT Sphere t
let sphereM m = shapeM Sphere m
let defaultPlane () = defaultShape Plane
let unitCube () = defaultShape Cube
let defaultCylinder () = defaultShape Cylinder
let defaultOpenCylinder () = defaultShape OpenCylinder
