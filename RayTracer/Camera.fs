module Camera

open System

open Matrix
open Ray
open Tuple

type Camera = {
  hSize: int
  vSize: int
  fov: float
  halfWidth: float
  halfHeight: float
  transform: Matrix
  pixelSize: float
}
let camera hSize vSize fov =
  let halfView = fov / 2. |> Math.Tan
  let aspect = (float hSize) / (float vSize)
  let (halfW, halfH) =
    if (aspect >= 1.) then
      (halfView, halfView / aspect)
    else (halfView * aspect, halfView)
  let pixelSize = (2. * halfW) / (float hSize)
  {
    hSize = hSize
    vSize = vSize
    fov = fov
    halfWidth = halfW
    halfHeight = halfH
    transform = (identity ())
    pixelSize = pixelSize
  }

let rayForPixel x y c =
  let xOffset = (float x + 0.5) * c.pixelSize
  let yOffset = (float y + 0.5) * c.pixelSize
  let worldX = c.halfWidth - xOffset
  let worldY = c.halfHeight - yOffset
  let pixel = multiplyT (inverse c.transform) (point worldX worldY -1.)
  let origin = multiplyT (inverse c.transform) (point 0. 0. 0.)
  let direction = pixel - origin |> normalize
  ray origin direction
