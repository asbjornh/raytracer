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
  | Cube ->
      let (xtmin, xtmax) = checkAxis ray.origin.X ray.direction.X
      let (ytmin, ytmax) = checkAxis ray.origin.Y ray.direction.Y
      let (ztmin, ztmax) = checkAxis ray.origin.Z ray.direction.Z

      let tmin = List.max [xtmin; ytmin; ztmin]
      let tmax = List.min [xtmax; ytmax; ztmax]

      if tmin > tmax then []
      else [(tmin, s); (tmax, s)]
  | TestShape -> [(0., s)]

let localNormal p (s: Shape) =
  match s.shape with
  | Sphere -> p - (point 0. 0. 0.)
  | Plane -> vector 0. 1. 0.
  | TestShape -> p
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

let sphere t m = {
  transform = t
  material = m
  shape = Sphere
}
let unitSphere () = sphere (identity ()) (defaultMaterial ())
let sphereT t = sphere t (defaultMaterial ())
let sphereM m = sphere (identity ()) m


let plane t m = { transform = t; material = m; shape = Plane }
let defaultPlane () = plane (identity ()) (defaultMaterial ())

let checkAxis origin direction =
  let tmin = (-1. - origin) / direction
  let tmax = (1. - origin) / direction
  (min tmax tmin, max tmax tmin)

let cube t m = { transform = t; material = m; shape = Cube }
let unitCube () = cube <| identity () <| defaultMaterial ()
