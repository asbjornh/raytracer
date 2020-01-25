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

let withNormalMap texturePath uvTransform mat =
  NormalMap {
    mat = mat
    tex = Texture.read texturePath
    transform = identity
    uvTransform = uvTransform
  }

let metalGate ambient diffuse uvTransform =
  withNormalMap "../tex/metal-gate/metal-gate-normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = None
      alpha = Some <| Texture.read "../tex/metal-gate/metal-gate-alpha.jpg"
      color = Texture.read "../tex/metal-gate/metal-gate-color.jpg"
      diffuse = diffuse
      specularMap = Some <| Texture.read "../tex/metal-gate/metal-gate-specular.jpg"
      specular = white
      shininess = 3.
      transform = identity
      uvTransform = uvTransform
    }

let metalGrill ambient diffuse uvTransform =
  withNormalMap "../tex/metal-grill/metal-grill-normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = Some <| Texture.read "../tex/metal-grill/metal-grill-ao.jpg"
      alpha = None
      color = Texture.read "../tex/metal-grill/metal-grill-color.jpg"
      diffuse = diffuse
      specularMap = Some <| Texture.read "../tex/metal-grill/metal-grill-specular.jpg"
      specular = white
      shininess = 3.
      transform = identity
      uvTransform = uvTransform
    }

let meat ambient diffuse uvTransform =
  withNormalMap "../tex/meat/meat-normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = None
      alpha = None
      color = Texture.read "../tex/meat/meat-color.jpg"
      diffuse = diffuse
      specular = white
      specularMap = Some <| Texture.read "../tex/meat/meat-specular.jpg"
      shininess = 2.
      transform = identity
      uvTransform = uvTransform
    }

let paddedFabric ambient diffuse uvTransform =
  withNormalMap "../tex/padded-fabric/padded-fabric-normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = Some <| Texture.read "../tex/padded-fabric/padded-fabric-ao.jpg"
      alpha = None
      color = Texture.read "../tex/padded-fabric/padded-fabric-color.jpg"
      diffuse = diffuse
      specular = white
      specularMap = Some <| Texture.read "../tex/padded-fabric/padded-fabric-specular.jpg"
      shininess = 5.
      transform = identity
      uvTransform = uvTransform
    }
