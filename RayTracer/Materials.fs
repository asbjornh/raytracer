module Materials

open Color
open Matrix
open Material

let luminance intensity texturePath uvTransform =
  Textured {
    ambient = intensity
    ambientOcclusion = None
    alpha = None
    color = Texture.read texturePath
    diffuse = 0.
    specularMap = None
    specular = black
    shininess = 0.
    transform = identity
    uvTransform = uvTransform
  }

let carPaint color specular shininess uvTransform =
  Blend {
    a = material white 0. 0. 1. // For specular highlight
    b = Textured {
      ambient = 0.
      ambientOcclusion = None
      alpha = None
      color = Texture.solid color
      diffuse = 0.7
      specularMap = Some <| Texture.read "../tex/noise-light.jpg"
      specular = specular
      shininess = shininess
      transform = identity
      uvTransform = uvTransform
    }
    mode = Add
  }

let reflection intensity texturePath uvTransform fresnelInner fresnelOuter =
  Fresnel {
    a = material black 0. 0. 0.
    b = luminance intensity texturePath uvTransform
    power = 2.
    mixInner = fresnelInner
    mixOuter = fresnelOuter
  }

let metalGate ambient diffuse uvTransform = NormalMap {
  mat = Textured {
    ambient = ambient
    ambientOcclusion = None
    alpha = Some <| Texture.read "../tex/metal-gate-alpha.jpg"
    color = Texture.read "../tex/metal-gate-color.jpg"
    diffuse = diffuse
    specularMap = Some <| Texture.read "../tex/metal-gate-specular.jpg"
    specular = white
    shininess = 3.
    transform = identity
    uvTransform = uvTransform
  }
  tex = Texture.read "../tex/metal-gate-normal.jpg"
  transform = identity
  uvTransform = uvTransform
}