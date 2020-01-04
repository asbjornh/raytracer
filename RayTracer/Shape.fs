module rec Shape

open System.Numerics
open Matrix
open Material
open Triangle
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
  | Triangle of Triangle

[<StructuredFormatDisplay("{AsString}")>]
type Shape =
  { mutable transform: Matrix4x4
    mutable material: Material
    shape: ShapeType
    mutable parent: Shape option
    mutable children: Shape list }
  member this.AsString =
    match this.shape with
    | Group g -> sprintf "Group '%s' {%A}" g.name this.children
    | Triangle t ->
      sprintf "Triangle %A" <| [t.p1; t.p2; t.p3]
    | t -> t.ToString ()

type Group = {
  name: string
  bounds: (float32 * float32) * (float32 * float32) * (float32 * float32)
}

let localIntersect ray (s: Shape) =
  match s.shape with
  | Sphere -> Sphere.intersect ray s
  | Plane -> Plane.intersect ray s
  | Cylinder -> Cylinder.intersect ray s
  | OpenCylinder -> Cylinder.intersectOpen ray s
  | Cube -> Cube.intersect ray s
  | Cone -> Cone.intersect -1.f 0.f ray s
  | DoubleCone -> Cone.intersect -1.f 1.f ray s
  | TestShape -> [(0.f, s)]
  | Triangle p -> Triangle.intersect p ray |> List.map (fun t -> (t, s))
  | Group g ->
    let (boundsX, boundsY, boundsZ) = g.bounds
    match Cube.intersectBox boundsX boundsY boundsZ ray s with
    | [] -> []
    | _ -> List.collect (shapeIntersect ray) s.children

let shapeIntersect ray (shape: Shape) =
  localIntersect
  <| Ray.transform (inverse shape.transform) ray
  <| shape

let uvAt p s =
  match s.shape with
  | Sphere -> Sphere.uv p
  | Plane -> Plane.uv p
  | TestShape -> (0.f, 0.f)
  | Cylinder -> failwith "Missing UV implementation for Plane"
  | OpenCylinder -> failwith "Missing UV implementation for OpenCylinder"
  | Cone -> failwith "Missing UV implementation for Cone"
  | DoubleCone -> failwith "Missing UV implementation for DoubleCone"
  | Cube -> failwith "Missing UV implementation for Cube"
  | Group _ -> failwith "Missing UV implementation for Group"
  | Triangle _ -> failwith "Missing UV implementation for Poly"

let localNormal (s: Shape) p =
  match s.shape with
  | Sphere -> p - (point 0. 0. 0.)
  | Plane -> vector 0. 1. 0.
  | TestShape -> p
  | Cylinder -> Cylinder.normal p
  | OpenCylinder -> vector32 p.X 0.f p.Z
  | Cone -> Cone.normal -1.f 0.f p
  | DoubleCone -> Cone.normal -1.f 1.f p
  | Cube -> Cube.normal p
  | Triangle p -> p.normal
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
    point32 minX maxY maxZ; point32 minX maxY minZ;
    point32 maxX maxY maxZ; point32 maxX maxY minZ;
    point32 minX minY maxZ; point32 minX minY minZ;
    point32 maxX minY maxZ; point32 maxX minY minZ;
  ]

let boundsForShape s =
  let cube = bounds (-1.f, 1.f) (-1.f, 1.f) (-1.f, 1.f)
  match s.shape with
  | Sphere -> cube
  | Plane -> bounds (-1.f, 1.f) (0.f, 0.f) (-1.f, 1.f)
  | TestShape -> []
  | Cylinder -> cube
  | OpenCylinder -> cube
  | Cone -> bounds (-1.f, 1.f) (-1.f, 0.f) (-1.f, 1.f)
  | DoubleCone -> cube
  | Cube -> cube
  | Triangle p ->
    let (x, y, z) = Triangle.bounds p
    bounds x y z
  | Group g ->
    let (x, y, z) = g.bounds
    bounds x y z

let boundsForShapes (objects: Shape list) =
  objects
  |> List.collect (fun s ->
    s |> boundsForShape |> List.map (multiplyT s.transform)
  )

let boundingBox (objects: Shape list) =
  if List.isEmpty objects
  then ( (0.f, 0.f), (0.f, 0.f), (0.f, 0.f) )
  else
    let (xs, ys, zs) =
      objects |> boundsForShapes |> List.map toXYZ |> List.unzip3
    ( (List.min xs, List.max xs),
      (List.min ys, List.max ys),
      (List.min zs, List.max zs) )

let updateParent parent shape =
  let newS = { shape with parent = Some parent }
  newS.children |> List.iter (fun s ->
    s.parent <- Some newS
  )
  newS


let shape s t m = {
  transform = t
  material = m
  shape = s
  parent = None
  children = []
}
let namedGroup n c t m =
  let g = Group { name = n; bounds = boundingBox c }
  let s = shape g t m
  s.children <- c |> List.map (updateParent s)
  s

let group c t m = namedGroup "N/A" c t m
let shapeM s m = shape s identity m
let shapeT s t = shape s t <| defaultMaterial ()
let defaultShape s = shape s identity <| defaultMaterial ()
let sphere t = shape Sphere t
let plane t = shape Plane t
let cube t = shape Cube t
let cylinder t = shape Cylinder t
let cone t = shape Cone t
let doubleCone t = shape DoubleCone t
let openCylinder t = shape OpenCylinder t
let triangle p1 p2 p3 t =
  let p = Triangle.make p1 p2 p3
  shape (Triangle p) t

let unitSphere () = defaultShape Sphere
let sphereT t = shapeT Sphere t
let sphereM m = shapeM Sphere m
let defaultPlane () = defaultShape Plane
let unitCube () = defaultShape Cube
let defaultCylinder () = defaultShape Cylinder
let defaultOpenCylinder () = defaultShape OpenCylinder
let defaultCone () = defaultShape Cone
let defaultDoubleCone () = defaultShape DoubleCone
let triangleT p1 p2 p3 t = shapeT (Triangle <| Triangle.make p1 p2 p3) t
let defaultTriangle p1 p2 p3 = defaultShape (Triangle <| Triangle.make p1 p2 p3)
let groupT c t = group c t <| defaultMaterial ()
let namedGroupT n c t = namedGroup n c t  <| defaultMaterial ()
let defaultGroup c = groupT c identity


let fanTriangulation = function
  | [] -> []
  | start :: rest ->
    List.pairwise rest
    |> List.map (fun (second, third) ->
      defaultTriangle start second third
    )

let polys (vs: Tuple list) face =
  match face with
  | [one; two; three] -> 
    defaultTriangle vs.[one] vs.[two] vs.[three]
  | _ ->
    face
    |> List.map (fun i -> vs.[i])
    |> fanTriangulation
    |> namedGroupT "Poly" <| identity
