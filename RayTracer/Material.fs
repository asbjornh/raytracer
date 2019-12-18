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

type Phong = {
  mutable color: Color
  mutable ambient: float
  mutable diffuse: float
  mutable specular: float
  mutable shininess: float
  mutable pattern: IPattern option
}

let defaultMaterial () = Phong (defaultMaterialP ())

let defaultMaterialP () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
  pattern = None
}

let material color ambient diffuse specular =
  Phong {
    defaultMaterialP () with
      color = color
      ambient = ambient
      diffuse = diffuse
      specular = specular
  }

let patternMaterialP pattern ambient diffuse specular =
  {
    defaultMaterialP () with
      ambient = ambient
      diffuse = diffuse
      specular = specular
      pattern = Some pattern
  }

let patternMaterial pattern ambient diffuse specular =
  Phong <| patternMaterialP pattern ambient diffuse specular

let materialC color =
  Phong { defaultMaterialP () with color = color }

let phongLighting
  (light: PointLight)
  (pos: Tuple)
  (eyeV: Tuple)
  (normalV: Tuple)
  (mat: Phong)
  (objectT: Matrix.Matrix)
  (inShadow: bool) =
    let overPoint = pos + (epsilon * normalV)
    let baseColor =
      match mat.pattern with
      | Some p -> colorAt overPoint objectT p
      | None -> mat.color
    let effectiveColor = multiply baseColor light.intensity
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

let constantLighting l _ _ _ _ _ _ = l.intensity

let lighting light =
  match light with
  | ConstantLight l -> constantLighting l
  | PointLight l -> phongLighting l

let fresnelShade a b normalV eyeV =
  let ang = angle (normalize normalV) (normalize eyeV)
  let mapping = pow 3.
  let max = mapping (Math.PI / 2.)
  let amount = ang |> mapping |> rangeMap (0., max) (0., 1.)
  blend a b amount
