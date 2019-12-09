module Material

open Color
open Light
open Tuple
open Util

type Material = {
  mutable color: Color;
  mutable ambient: float;
  mutable diffuse: float;
  mutable specular: float;
  mutable shininess: float;
}

let material () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
}

let lighting light pos eyeV normalV mat =
  let effectiveColor = multiply mat.color light.intensity
  let lightV = normalize (light.position - pos)
  let lightDotNormal = dot lightV normalV
  let ambient = scale mat.ambient effectiveColor

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
