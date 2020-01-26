module Materials

open Color
open Matrix
open Material

let specularOnly specular shininess = layerMaterial white 0. 0. specular shininess

let coloredMetal color specular shininess blurOptions = Blend {
  mode = Overlay
  a = Blend {
    mode = Add
    a = Reflective blurOptions
    b = specularOnly specular shininess
  }
  b = Luminance color
}
let gold = coloredMetal Color.gold
let brass = coloredMetal (mix Color.gold (gray 0.5) 0.4)

let coloredGlass color =
  Blend {
    mode = Add
    a = specularOnly 1. 200.
    b = Blend {
      mode = Multiply
      a = Luminance color
      b = Transparent { index = 1.5f }
    }
  }

let luminanceTex texturePath uvTransform =
  LuminanceTexture {
    tex = Texture.read texturePath
    transform = identity
    uvTransform = uvTransform
  }

let specularHighlight color ambient diffuse specular shininess texturePath uvTransform =
  Textured {
    ambient = ambient
    ambientOcclusion = None
    alpha = None
    color = Texture.solid color
    diffuse = diffuse
    specularMap = Some <| Texture.read texturePath
    specular = specular
    shininess = shininess
    transform = identity
    uvTransform = uvTransform
  }

let carPaint color specular uvTransform =
  Blend {
    mode = Add
    a = specularOnly 1. 200.
    b = specularHighlight color 0. 0.7 specular 2. "../tex/noise-light.jpg" uvTransform
  }

let glitterHighlight color ambient diffuse specular uvTransform =
  specularHighlight color ambient diffuse (scale specular white) 1.5 "../tex/glitter.jpg" uvTransform

let reflectionTex texturePath uvTransform fresnelInner fresnelOuter =
  Fresnel {
    a = Luminance black
    b = luminanceTex texturePath uvTransform
    blend = Normal
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

let techno ambient diffuse uvTransform =
  withNormalMap "../tex/techno/techno-normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = Some <| Texture.read "../tex/techno/techno-ao.jpg"
      alpha = None
      color = Texture.read "../tex/techno/techno-color.jpg"
      diffuse = diffuse
      specular = white
      specularMap = Some <| Texture.read "../tex/techno/techno-specular.jpg"
      shininess = 5.
      transform = identity
      uvTransform = uvTransform
    }

let metalGrill2 ambient diffuse uvTransform =
  withNormalMap "../tex/metal-grill2/normal.jpg" uvTransform
    <| Textured {
      ambient = ambient
      ambientOcclusion = Some <| Texture.read "../tex/metal-grill2/ao.jpg"
      alpha = Some <| Texture.read "../tex/metal-grill2/alpha.jpg"
      color = Texture.read "../tex/metal-grill2/color.jpg"
      diffuse = diffuse
      specular = white
      specularMap = Some <| Texture.read "../tex/metal-grill2/specular.jpg"
      shininess = 50.
      transform = identity
      uvTransform = uvTransform
    }
