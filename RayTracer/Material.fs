module rec Material

open System
open System.Numerics

open Color
open Light
open Pattern
open Tuple
open Util

type Material =
  | Phong of Phong
  | Reflective of Reflective
  | Fresnel of Fresnel
  | Mix of Mix
  | Blend of Blend
  | Pattern of Pattern
  | Gradient of Gradient
  | Transparent of Transparent
  | Textured of Textured
  | NormalMap of NormalMap
  | Luminance of Color
  | InvisFloor of InvisFloor
  | TestPattern

type InvisFloor = {
  shadowColor: Color
}

type Fresnel = {
  a: Material
  b: Material
  power: float
  mixInner: float
  mixOuter: float
}

type Mix = {
  a: Material
  b: Material
  mix: float
}

type Blend = {
  a: Material
  b: Material
  mode: BlendingMode
}

type Textured = {
  ambient: float
  alpha: (Color list list) option
  color: Color list list
  diffuse: float
  specularMap: (Color list list) option
  specular: float
  shininess: float
  transform: Matrix4x4
  // NOTE: (uScale * vScale * uOffset * vOffset)
  uvTransform: float * float * float * float
}

type NormalMap = {
  mat: Material
  tex: Color list list
  transform: Matrix4x4
  uvTransform: float * float * float * float
}

type Reflective = {
  blend: BlendingMode
}

type Transparent = {
  index: float32
  blend: BlendingMode
}

type Gradient = {
  a: Material
  b: Material
  sharpness: float
  transform: Matrix4x4
}

type Pattern = {
  a: Material
  b: Material
  transform: Matrix4x4
  pattern: PatternType
}

type Phong = {
  mutable color: Color
  mutable ambient: float
  mutable diffuse: float
  mutable specular: float
  mutable shininess: float
}

let phongColor
  (baseColor: Color)
  (ambient: float)
  (diffuse: float)
  (specular: float)
  (shininess: float)
  (light: PointLight)
  (pos: Vector4)
  (eyeV: Vector4)
  (normalV: Vector4)
  (shadowAmount: float) =
    let effectiveColor = multiply baseColor light.intensity
    let ambient = scale ambient effectiveColor

    if (looseEq shadowAmount 1.)
    then ambient
    else
      let lightV = normalize (light.position - pos)
      let lightDotNormal = dot lightV normalV

      let (diffuse, specular) =
        if (lightDotNormal < 0.f)
        then (black, black)
        else
          let diffuse = effectiveColor |> scale diffuse |> scale (float lightDotNormal)
          let reflectV = reflect normalV (negate lightV)
          let reflectDotEye = dot eyeV reflectV

          if (reflectDotEye <= 0.f)
          then (diffuse, black)
          else
            let factor = pow shininess (float reflectDotEye)
            let specular = light.intensity |> scale factor |> scale specular
            (diffuse, specular)

      ambient |> add diffuse |> add specular

let phongLighting (light: PointLight) pos eyeV normalV mat shadowAmount =
    phongColor
    <| mat.color <| mat.ambient <| mat.diffuse <| mat.specular <| mat.shininess
    <| light <| pos <| eyeV <| normalV <| shadowAmount

let softLighting (light: SoftLight) pos eyeV normalV mat shadowAmount =
  let phongComponent = phongLighting light.light pos eyeV normalV mat 0.
  if (shadowAmount = 0.)
  then phongComponent
  else
    let shadowComponent = phongLighting light.light pos eyeV normalV mat 1.
    mix phongComponent shadowComponent shadowAmount

let constantLighting (l: ConstantLight) _ _ _ _ _ = l.intensity

let lighting light =
  match light with
  | ConstantLight l -> constantLighting l
  | PointLight l -> phongLighting l
  | SoftLight l -> softLighting l

let fresnelShade a b power normalV eyeV =
  let ang = angle (normalize normalV) (normalize eyeV) |> float
  let mapping = pow power
  let max = mapping (Math.PI / 2.)
  let amount = ang |> mapping |> rangeMap (0., max) (0., 1.)
  mix a b amount

// If either matA or matB is a Reflective, the non-reflective color is blended with the reflective one (using the blending mode in the reflective material)
let getBlendComponents matA matB a b =
  match (matA, matB) with
  | (Reflective mat, _) -> (blend mat.blend a b, b)
  | (_, Reflective mat) -> (a, blend mat.blend b a)
  | (Transparent mat, _) -> (blend mat.blend a b, b)
  | (_, Transparent mat) -> (a, blend mat.blend b a)
  | _ -> (a, b)

let defaultMaterial () = Phong (defaultMaterialP ())

let defaultMaterialP () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
}

let materialShiny shininess color ambient diffuse specular =
  Phong {
    color = color
    ambient = ambient
    diffuse = diffuse
    specular = specular
    shininess = shininess
  }

let material color = materialShiny 200. color

let materialC color =
  Phong { defaultMaterialP () with color = color }

let gradient a b t =
  Gradient { a = a; b = b; sharpness = 0.; transform = t }

let textureRaw path (uScale, vScale) (uOffset, vOffset) t =
  {
    alpha = None
    ambient = 0.1
    color = Texture.read path;
    diffuse = 0.9
    specularMap = None
    specular = 0.9
    shininess = 200.
    transform = t
    uvTransform = (uScale, vScale, uOffset, vOffset)
  }
let texture path (uScale, vScale) (uOffset, vOffset) t =
  textureRaw path (uScale, vScale) (uOffset, vOffset) t |> Textured
let textureT path = texture path (1., 1.) (0., 0.)
