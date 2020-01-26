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
  | Layer of Phong // NOTE: Ignores shadows
  | Reflective of BlurryReflection option
  | Fresnel of Fresnel
  | Mix of Mix
  | Blend of Blend
  | Pattern of Pattern
  | Gradient of Gradient
  | Transparent of Transparent
  | Textured of Textured
  | NormalMap of NormalMap
  | Luminance of Color
  | LuminanceTexture of LuminanceTexture
  | InvisFloor of InvisFloor
  | TestPattern

type InvisFloor = {
  shadowColor: Color
}

type Fresnel = {
  a: Material
  b: Material
  blend: BlendingMode
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
  ambientOcclusion: (Color list list) option
  alpha: (Color list list) option
  color: Color list list
  diffuse: float
  specularMap: (Color list list) option
  specular: Color
  shininess: float
  transform: Matrix4x4
  // NOTE: (uScale * vScale * uOffset * vOffset)
  uvTransform: float * float * float * float
}

type LuminanceTexture = {
  tex: Color list list
  // NOTE: (uScale * vScale * uOffset * vOffset)
  transform: Matrix4x4
  uvTransform: float * float * float * float
}

type NormalMap = {
  mat: Material
  tex: Color list list
  transform: Matrix4x4
  uvTransform: float * float * float * float
}

type BlurryReflection = {
  samples: int
  angle: float
}

type Transparent = {
  index: float32
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
  ambient: Color
  diffuse: Color
  specular: Color
  shininess: float
  softShadows: bool
}

let phongColor
  (ambient: Color)
  (diffuse: Color)
  (specular: Color)
  (shininess: float)
  (light: PointLight)
  (pos: Vector4)
  (eyeV: Vector4)
  (normalV: Vector4)
  (shadowAmount: float) =
    let ambient = multiply ambient light.intensity

    if (looseEq shadowAmount 1.)
    then ambient
    else
      let lightV = normalize (light.position - pos)
      let lightDotNormal = dot lightV normalV

      let (diffuse, specular) =
        if (lightDotNormal < 0.f)
        then (black, black)
        else
          let diffuse = multiply diffuse light.intensity |> scale (float lightDotNormal)
          let reflectV = reflect normalV (negate lightV)
          let reflectDotEye = dot eyeV reflectV

          if (reflectDotEye <= 0.f)
          then (diffuse, black)
          else
            let factor = pow shininess (float reflectDotEye)
            let specular = multiply specular light.intensity |> scale factor
            (diffuse, specular)

      ambient |> add diffuse |> add specular

let phongLighting (light: PointLight) pos eyeV normalV mat shadowAmount =
    phongColor
      mat.ambient mat.diffuse mat.specular mat.shininess
      light pos eyeV normalV shadowAmount

let softLighting (light: SoftLight) pos eyeV normalV mat shadowAmount =
  let phongComponent = phongLighting light.light pos eyeV normalV mat 0.
  if (shadowAmount = 0.)
  then phongComponent
  else
    let shadowComponent = phongLighting light.light pos eyeV normalV mat 1.
    mix phongComponent shadowComponent shadowAmount

let lighting light =
  match light with
  | PointLight l -> phongLighting l
  | SoftLight l -> softLighting l

let fresnelShade a b power normalV eyeV =
  let ang = angle (normalize normalV) (normalize eyeV) |> float
  let mapping = pow power
  let max = mapping (Math.PI / 2.)
  let amount = ang |> mapping |> rangeMap (0., max) (0., 1.)
  mix a b amount

let materialRaw ambient diffuse specular shininess =
  Phong {
    ambient = ambient
    diffuse = diffuse
    specular = specular
    shininess = shininess
    softShadows = true
  }

let layerMaterial color ambient diffuse specular shininess =
  Layer {
    ambient = (scale ambient color)
    diffuse = (scale diffuse color)
    specular = (scale specular color)
    shininess = shininess
    softShadows = true
  }

let defaultMaterialP () = {
  ambient = scale 0.1 white
  diffuse = scale 0.9 white
  specular = scale 0.9 white
  shininess = 200.
  softShadows = true
}

let defaultMaterial () = Phong (defaultMaterialP ())

let materialShiny color ambient diffuse specular shininess =
  materialRaw
    (scale ambient color)
    (scale diffuse color)
    specular
    shininess
let material color ambient diffuse specular =
  materialRaw
    (scale ambient color)
    (scale diffuse color)
    (scale specular color)
    200.

let materialSharp color ambient diffuse specular =
  Phong {
    ambient = (scale ambient color)
    diffuse = (scale diffuse color)
    specular = (scale specular color)
    shininess = 200.
    softShadows = false
  }

let materialC color =
  material color 0.1 0.9 0.9

let gradient a b t =
  Gradient { a = a; b = b; sharpness = 0.; transform = t }

let textureRaw path (uScale, vScale) (uOffset, vOffset) t =
  {
    alpha = None
    ambient = 0.1
    ambientOcclusion = None
    color = Texture.read path;
    diffuse = 0.9
    specularMap = None
    specular = scale 0.9 white
    shininess = 200.
    transform = t
    uvTransform = (uScale, vScale, uOffset, vOffset)
  }
let texture path (uScale, vScale) (uOffset, vOffset) t =
  textureRaw path (uScale, vScale) (uOffset, vOffset) t |> Textured
let textureT path = texture path (1., 1.) (0., 0.)
