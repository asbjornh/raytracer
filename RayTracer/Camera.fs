module Camera

open System
open System.Numerics
open ShellProgressBar

open Canvas
open Color
open Matrix
open Ray
open Transform
open Tuple
open Util
open World

type Camera = {
  hSize: int
  vSize: int
  fov: float32
  halfWidth: float32
  halfHeight: float32
  mutable transform: Matrix4x4
  pixelSize: float32
}
let camera hSize vSize fov =
  let halfView = fov / 2.f |> MathF.Tan
  let aspect = (float32 hSize) / (float32 vSize)
  let (halfW, halfH) =
    if (aspect >= 1.f) then
      (halfView, halfView / aspect)
    else (halfView * aspect, halfView)
  let pixelSize = (2.f * halfW) / (float32 hSize)
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
  let xOffset = (float32 x + 0.5f) * c.pixelSize
  let yOffset = (float32 y + 0.5f) * c.pixelSize
  let worldX = c.halfWidth - xOffset
  let worldY = c.halfHeight - yOffset
  let pixel = multiplyT (inverse c.transform) (point32 worldX worldY -1.f)
  let origin = multiplyT (inverse c.transform) (point32 0.f 0.f 0.f)
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
      let o = occlusionAt point normalV samples |> float |> (*) 0.5
      add black (Color.scale o white)
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

let aaTransforms =
  [ translateX (0.0035f)
    translateX (-0.0035f)
    translateY (0.0035f)
    translateY (-0.0035f) ]

let renderAA (c: Camera) w =
  let canv = canvas c.hSize c.vSize
  let len = 4 * Canvas.length canv

  let bar = new ProgressBar (len, "Rendering", ConsoleColor.Yellow)

  let result = canv |> Canvas.render (fun x y ->
    let (rs, gs, bs) =
      aaTransforms
      |> List.map (fun t ->
        bar.Tick (sprintf "Rendering %i pixels" len)
        rayForPixel x y c
        |> Ray.transform t
        |> colorAt w <| 4
        |> (fun c -> c.Return) )
      |> List.unzip3
    color <| List.average rs <| List.average gs <| List.average bs
  )

  printfn "\n" // To avoid CLI glitch after rendering
  result
