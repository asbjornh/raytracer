module rec Shape

open Matrix
open Material
open Ray
open Tuple
open Util

type IShape =
  abstract Transform: Matrix
  abstract Material: Material
  abstract SetMaterial: Material -> unit
  abstract LocalIntersect: Ray -> (float * IShape) list
  abstract LocalNormal: Tuple -> Tuple

let shapeIntersect ray (shape: IShape) =
  shape.LocalIntersect
  <| Ray.transform (inverse shape.Transform) ray

let normalAt point (shape: IShape) =
  let invT = inverse shape.Transform
  let localP = multiplyT invT point
  let localN = shape.LocalNormal localP
  let worldN = multiplyT (transpose invT) localN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)


type Sphere =
  {
    Transform: Matrix;
    mutable Material: Material
  }
  interface IShape with
    member this.Transform = this.Transform
    member this.Material = this.Material
    member this.SetMaterial mat = this.Material <- mat
    member this.LocalIntersect r = sphereIntersect r this
    member this.LocalNormal t = t - (point 0. 0. 0.)

let sphere t m: Sphere = {
  Transform = t
  Material = m
}
let unitSphere () = sphere (identity ()) (defaultMaterial ())
let sphereT t = sphere t (defaultMaterial ())
let sphereM m = sphere (identity ()) m

let sphereIntersect (ray: Ray) (s: IShape) =
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


type Plane =
  {
    Transform: Matrix
    mutable Material: Material
  }
  interface IShape with
    member this.Transform = this.Transform
    member this.SetMaterial mat = this.Material <- mat
    member this.Material = this.Material
    member this.LocalIntersect r =
      if (looseEq r.direction.Y 0.)
      then []
      else
        let t = -r.origin.Y / r.direction.Y
        [(t, (this :> IShape))]
    member this.LocalNormal _ = (vector 0. 1. 0.)

let plane t m : Plane = { Transform = t; Material = m }

let defaultPlane () = plane (identity ()) (defaultMaterial ())

type Cube =
  {
    Transform: Matrix
    mutable Material: Material
  }
  interface IShape with
    member this.Transform = this.Transform
    member this.Material = this.Material
    member this.SetMaterial mat = this.Material <- mat
    member this.LocalIntersect ray =
      let (xtmin, xtmax) = checkAxis ray.origin.X ray.direction.X
      let (ytmin, ytmax) = checkAxis ray.origin.Y ray.direction.Y
      let (ztmin, ztmax) = checkAxis ray.origin.Z ray.direction.Z

      let tmin = List.max [xtmin; ytmin; ztmin]
      let tmax = List.min [xtmax; ytmax; ztmax]

      [(tmin, this :> IShape); (tmax, this :> IShape)]
    member this.LocalNormal _ = vector 0. 0. 0.

let checkAxis origin direction =
  let tminNumerator = (-1. - origin)
  let tmaxNumerator = (1. - origin)

  let (tmin, tmax) =
    if (abs direction) >= epsilon then
      (tminNumerator / direction, tmaxNumerator / direction)
    else
      (tminNumerator * infinity, tmaxNumerator * infinity)

  if tmin > tmax then (tmax, tmin) else (tmin, tmax)

let cube t m : Cube = { Transform = t; Material = m }

let unitCube () = cube <| identity () <| defaultMaterial ()

let assignMaterial (s: IShape) (m: Material) =
  match s with
  | :? Sphere as s -> sphere s.Transform m :> IShape
  | :? Plane as p -> plane p.Transform m :> IShape
  | _ -> s

