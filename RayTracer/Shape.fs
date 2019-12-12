module rec Shape

open Matrix
open Material
open Ray
open Tuple
open Util

type IShape =
  abstract Transform: Matrix
  abstract Material: Material
  abstract Intersect: Ray -> (float * IShape) list

let normal (p: Tuple) (s: IShape) =
  let invT = inverse s.Transform
  let objectN = (multiplyT invT p) - (point 0. 0. 0.)
  let worldN = multiplyT (transpose invT) objectN
  let (x, y, z, _) = worldN.Return
  normalize (vector x y z)


type Sphere =
  {
    Transform: Matrix;
    Material: Material
  }
  interface IShape with
    member this.Transform = this.Transform
    member this.Material = this.Material
    member this.Intersect r = intersectSphere r this

let sphere t m: Sphere = {
  Transform = t
  Material = m
}
let unitSphere () = sphere (identity ()) (defaultMaterial ())
let sphereT t = sphere t (defaultMaterial ())
let sphereM m = sphere (identity ()) m

let intersectSphere (ray: Ray) (s: IShape) =
  let r = Ray.transform (inverse s.Transform) ray
  let sphereToRay = r.origin - (point 0. 0. 0.)
  let a = dot r.direction r.direction
  let b = 2. * (dot r.direction sphereToRay)
  let c = (dot sphereToRay sphereToRay) - 1.
  let discriminant = pow 2. b - 4. * a * c
  if (discriminant < 0.)
  then []
  else
    let t1 = (-b - sqrt discriminant) / (2. * a)
    let t2 = (-b + sqrt discriminant) / (2. * a)
    [(t1, s); (t2, s)]
