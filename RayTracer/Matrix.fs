module Matrix

open System.Numerics

open Util

let matrix (a: float list list) =
  match List.map (List.map float32) a with
  | [ [ m11; m12; m13; m14 ]
      [ m21; m22; m23; m24 ]
      [ m31; m32; m33; m34 ]
      [ m41; m42; m43; m44 ]
    ] ->
      Matrix4x4 (m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44)
  | _ -> failwith "list must be 4x4"

let values (m: Matrix4x4) =
  [ m.M11; m.M12; m.M13; m.M14
    m.M21; m.M22; m.M23; m.M24
    m.M31; m.M32; m.M33; m.M34
    m.M41; m.M42; m.M43; m.M44
  ]

let equals a b =
  List.fold2 (fun acc a b -> acc && looseEq32 a b) true (values a) (values b)

let identity = Matrix4x4.Identity

let multiply (a: Matrix4x4) = flip (*) a

let transpose (a: Matrix4x4) =
  Matrix4x4.Transpose a

let determinant (m: Matrix4x4) =
  m.GetDeterminant ()

let invertible (m: Matrix4x4) = (determinant m) <> 0.f

let inverse (m: Matrix4x4) =
  match Matrix4x4.Invert m with
  | (true, m) -> m
  | (false, _) -> failwith <| sprintf "Matrix inversion failed for %A" m
