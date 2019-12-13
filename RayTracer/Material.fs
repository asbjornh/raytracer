module Material

open Color
open Light
open Pattern
open Tuple
open Util

type Material = {
  mutable color: Color;
  mutable ambient: float;
  mutable diffuse: float;
  mutable specular: float;
  mutable shininess: float;
  mutable pattern: IPattern option
}

let defaultMaterial () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
  pattern = None
}

let material color ambient diffuse specular =
  {
    defaultMaterial () with
      color = color
      ambient = ambient
      diffuse = diffuse
      specular = specular
  }

let patternMaterial pattern ambient diffuse specular =
  {
    defaultMaterial () with
      ambient = ambient
      diffuse = diffuse
      specular = specular
      pattern = Some pattern
  }

let materialC color =
  { defaultMaterial () with color = color }

let phongLighting (light: PointLight) pos eyeV normalV mat inShadow =
  let baseColor =
    match mat.pattern with
    | Some p -> p.GetColor pos
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

let lighting light pos eyeV normalV mat inShadow =
  match light with
  | ConstantLight l -> l.intensity
  | PointLight l -> phongLighting l pos eyeV normalV mat inShadow
