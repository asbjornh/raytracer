module Camera

open System
open ShellProgressBar

open Canvas
open Color
open Matrix
open Ray
open Tuple
open Util
open World

type Camera = {
  hSize: int
  vSize: int
  fov: float
  halfWidth: float
  halfHeight: float
  mutable transform: Matrix
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

let render c w =
  (canvas c.hSize c.vSize) |> Canvas.render (fun x y ->
    rayForPixel x y c |> colorAt w <| 4
  )

let renderOcclusion c w =
  let canv = canvas c.hSize c.vSize
  let len = Canvas.length canv
  let bar = new ProgressBar (2, "Rendering", ConsoleColor.Yellow)

  bar.Tick ()
  let renderBar = bar.Spawn (len, "Raytracing")

  let result = canv |> Canvas.map (fun x y ->
    renderBar.Tick (sprintf "Raytracing %i pixels" len)
    rayForPixel x y c |> colorAndDepthAt w <| 4
  )

  bar.Tick ()
  let occlusionBar = bar.Spawn(len, "Processing")
  let pixels = map2d (fun (_, _, c) -> c) result
  let depths =
    result |> map2d (fun (p, n, _) -> (p, n))
  let occlusion =
    depths
    |> mapi2d (fun x y (point, normalV) ->
      occlusionBar.Tick (sprintf "Processing %i pixels" len)
      let samples = depths |> subGrid x y 5 |> Array.concat
      let o = occlusionAt point normalV samples |> (*) 0.5
      add black (scale o white)
    )

  printfn "\n" // To avoid CLI glitch after rendering
  map22d subtract pixels occlusion

let renderProgress (c: Camera) w =
  let canv = canvas c.hSize c.vSize
  let len = Canvas.length canv

  let bar = new ProgressBar (len, "Rendering", ConsoleColor.Yellow)

  let result = canv |> Canvas.render (fun x y ->
    bar.Tick (sprintf "Rendering %i pixels" len)
    rayForPixel x y c |> colorAt w <| 4
  )

  printfn "\n" // To avoid CLI glitch after rendering
  result
