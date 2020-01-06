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
  | TestPattern

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
  tex: Color list list
  uScale: float
  uOffset: float
  vScale: float
  vOffset: float
  transform: Matrix4x4
}

type NormalMap = {
  mat: Material
  tex: Textured
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

let phongLighting
  (light: PointLight)
  (pos: Tuple)
  (eyeV: Tuple)
  (normalV: Tuple)
  (mat: Phong)
  (shadowAmount: float) =
    let effectiveColor = multiply mat.color light.intensity
    let ambient = scale mat.ambient effectiveColor

    if (looseEq shadowAmount 1.)
    then ambient
    else
      let lightV = normalize (light.position - pos)
      let lightDotNormal = dot lightV normalV

      let (diffuse, specular) =
        if (lightDotNormal < 0.f)
        then (black, black)
        else
          let diffuse = effectiveColor |> scale mat.diffuse |> scale (float lightDotNormal)
          let reflectV = reflect normalV (negate lightV)
          let reflectDotEye = dot eyeV reflectV

          if (reflectDotEye <= 0.f)
          then (diffuse, black)
          else
            let factor = pow mat.shininess (float reflectDotEye)
            let specular = light.intensity |> scale factor |> scale mat.specular
            (diffuse, specular)

      ambient |> add diffuse |> add specular

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

let material color ambient diffuse specular =
  Phong {
    defaultMaterialP () with
      color = color
      ambient = ambient
      diffuse = diffuse
      specular = specular
  }

let materialC color =
  Phong { defaultMaterialP () with color = color }

let gradient a b t =
  Gradient { a = a; b = b; sharpness = 0.; transform = t }

let textureRaw path (uScale, vScale) (uOffset, vOffset) t =
  {
    tex = Texture.read path;
    transform = t
    uOffset = uOffset
    uScale = uScale
    vOffset = vOffset
    vScale = vScale
  }
let texture path (uScale, vScale) (uOffset, vOffset) t =
  textureRaw path (uScale, vScale) (uOffset, vOffset) t |> Textured
let textureT path = texture path (1., 1.) (0., 0.)
