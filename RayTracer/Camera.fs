module Camera

open System

open Matrix

type Camera = {
  hSize: int
  vSize: int
  fov: float
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
    transform = (identity ())
    pixelSize = pixelSize
  }