module rec Shape

open Matrix
open Material
open Ray
open Tuple
open Util

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
  | Sphere ->
      let sphereToRay = ray.origin - (point 0. 0. 0.)
      let a = dot ray.direction ray.direction
      let b = 2. * (dot ray.direction sphereToRay)
      let c = (dot sphereToRay sphereToRay) - 1.
      let discriminant = pow 2. b - 4. * a * c
      if (discriminant < 0.)
      then []
      else
        let t1 = (-b - sqrt discriminant) / (2. * a)
        let t2 = (-b + sqrt discriminant) / (2. * a)
        [(t1, s); (t2, s)]
  | Plane ->
      if (looseEq ray.direction.Y 0.)
      then []
      else [(-ray.origin.Y / ray.direction.Y, s)]
  | Cylinder ->
      List.concat [
        intersectOpenCylinder ray s
        intersectCaps ray s
      ]
  | OpenCylinder -> intersectOpenCylinder ray s
  | Cube ->
      let (xtmin, xtmax) = checkAxis ray.origin.X ray.direction.X
      let (ytmin, ytmax) = checkAxis ray.origin.Y ray.direction.Y
      let (ztmin, ztmax) = checkAxis ray.origin.Z ray.direction.Z

      let tmin = List.max [xtmin; ytmin; ztmin]
      let tmax = List.min [xtmax; ytmax; ztmax]

      if tmin > tmax then []
      else [(tmin, s); (tmax, s)]
  | TestShape -> [(0., s)]

let cylMin = -1.
let cylMax = 1.
let intersectOpenCylinder ray s =
  let a = ray.direction.X ** 2. + ray.direction.Z ** 2.
  if a < epsilon then []
  else
    let b =
      2. * ray.origin.X * ray.direction.X +
      2. * ray.origin.Z * ray.direction.Z
    let c = ray.origin.X ** 2. + ray.origin.Z ** 2. - 1.
    let discriminant = b ** 2. - 4. * a * c
    if discriminant < 0. then []
    else
      let _t0 = (-b - sqrt discriminant) / (2. * a)
      let _t1 = (-b + sqrt discriminant) / (2. * a)
      let (t0, t1) = (max _t0 _t1, min _t0 _t1)
      let y0 = ray.origin.Y + t0 * ray.direction.Y
      let y1 = ray.origin.Y + t1 * ray.direction.Y
      let first =
        if (cylMin < y0 && y0 < cylMax) then [(t0, s)] else []
      let second =
        if (cylMin < y1 && y1 < cylMax) then [(t1, s)] else []
      List.concat [first; second]

let checkCap ray t =
  let x = ray.origin.X + t * ray.direction.X
  let z = ray.origin.Z + t * ray.direction.Z
  (x ** 2. + z ** 2.) <= 1.

let intersectCaps ray cyl =
  if looseEq ray.direction.Y 0. then []
  else
    let t1 = (cylMin - ray.origin.Y) / ray.direction.Y
    let t2 = (cylMax - ray.origin.Y) / ray.direction.Y
    let first =
      if checkCap ray t1 then [(t1, cyl)] else []
    let second =
      if checkCap ray t2 then [(t2, cyl)] else []
    List.concat [first; second]

let checkAxis origin direction =
  let tmin = (-1. - origin) / direction
  let tmax = (1. - origin) / direction
  (min tmax tmin, max tmax tmin)

let localNormal p (s: Shape) =
  match s.shape with
  | Sphere -> p - (point 0. 0. 0.)
  | Plane -> vector 0. 1. 0.
  | TestShape -> p
  | Cylinder ->
      let dist = p.X ** 2. + p.Z ** 2.
      if (dist < 1. && p.Y >= cylMax - epsilon)
      then vector 0. 1. 0.
      else if (dist < 1. && p.Y <= cylMin + epsilon)
      then vector 0. -1. 0.
      else vector p.X 0. p.Z
  | OpenCylinder -> vector p.X 0. p.Z
  | Cube ->
      let (x, y, z, _) = Tuple.Map abs p
      let maxC = List.max [x; y; z]
      if maxC = x then vector p.X 0. 0.
      else if maxC = y then vector 0. p.Y 0.
      else vector 0. 0. p.Z

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
