module rec Material

open System

open Color
open Light
open Pattern
open Tuple
open Util

type Material =
  | Phong of Phong
  | Reflective of Reflective
  | Fresnel of Fresnel
  | Blend of Blend
  | Pattern of Pattern
  | Gradient of Gradient

type Fresnel = {
  a: Material
  b: Material
  mix: float
}

type Blend = {
  a: Material
  b: Material
  mix: float
}

type Reflective = {
  additive: bool
}

type Gradient = {
  a: Material
  b: Material
  transform: Matrix.Matrix
}

type Pattern = {
  a: Material
  b: Material
  transform: Matrix.Matrix
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
  (inShadow: bool) =
    let effectiveColor = multiply mat.color light.intensity
    let ambient = scale mat.ambient effectiveColor

    if inShadow
    then ambient
    else
      let lightV = normalize (light.position - pos)
      let lightDotNormal = dot lightV normalV

      let (diffuse, specular) =
        if (lightDotNormal < 0.)
        then (black, black)
        else
          let diffuse = effectiveColor |> scale mat.diffuse |> scale lightDotNormal
          let reflectV = reflect normalV (negate lightV)
          let reflectDotEye = dot eyeV reflectV

          if (reflectDotEye <= 0.)
          then (diffuse, black)
          else
            let factor = pow mat.shininess reflectDotEye
            let specular = light.intensity |> scale factor |> scale mat.specular
            (diffuse, specular)

      ambient |> add diffuse |> add specular

let constantLighting l _ _ _ _ _ = l.intensity

let lighting light =
  match light with
  | ConstantLight l -> constantLighting l
  | PointLight l -> phongLighting l

let fresnelColor a b normalV eyeV =
  let ang = angle (normalize normalV) (normalize eyeV)
  let mapping = pow 3.
  let max = mapping (Math.PI / 2.)
  let amount = ang |> mapping |> rangeMap (0., max) (0., 1.)
  blend a b amount

let fresnelShade a b normalV eyeV matA matB =
  match (matA, matB) with
  | (Reflective r, _) when r.additive ->
    fresnelColor a black normalV eyeV |> add b
  | (_, Reflective r) when r.additive ->
    fresnelColor black b normalV eyeV |> add a
  | _ -> fresnelColor a b normalV eyeV


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