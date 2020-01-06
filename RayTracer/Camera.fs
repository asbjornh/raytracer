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
  transform: Matrix4x4
  pixelSize: float32
}
let cameraT hSize vSize fov t =
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
    transform = t
    pixelSize = pixelSize
  }

let camera hSize vSize fov position target =
  viewTransform position target (vector32 0.f 1.f 0.f)
  |> cameraT hSize vSize fov

let rayForPixel32 x y c =
  let xOffset = (x + 0.5f) * c.pixelSize
  let yOffset = (y + 0.5f) * c.pixelSize
  let worldX = c.halfWidth - xOffset
  let worldY = c.halfHeight - yOffset
  let pixel = multiplyT (inverse c.transform) (point32 worldX worldY -1.f)
  let origin = multiplyT (inverse c.transform) (point32 0.f 0.f 0.f)
  let direction = pixel - origin |> normalize
  ray origin direction

let rayForPixel x y c =
  rayForPixel32 (float32 x) (float32 y) c


// TODO: Make render effects composable
let renderCb c w fn =
  (canvas c.hSize c.vSize) |> Canvas.render (fun x y ->
    fn ()
    rayForPixel x y c |> colorAt w <| 4
  )
let render c w = renderCb c w ignore

let withProgressTxt len txt fn =
  let bar = new ProgressBar (len, txt len, ConsoleColor.Yellow)
  let result = fn (fun _ -> bar.Tick ())
  printfn "\n" // To avoid CLI glitch after rendering
  result

let withProgress len fn =
  let txt = sprintf "Rendering %i pixels"
  withProgressTxt len txt fn

let renderProgress (c: Camera) w =
  withProgress <| c.hSize * c.vSize <| renderCb c w

// TODO: Check occlusion at different distances
let renderOcclusion c w =
  let canv = canvas c.hSize c.vSize
  let len = Canvas.length canv

  let result = withProgress len <| (fun tick ->
    canv |> Canvas.map (fun x y ->
      tick ()
      rayForPixel x y c |> colorAndDepthAt w <| 4
    )
  )

  let pixels = map2d (fun (_, _, c) -> c) result
  let depths = result |> map2d (fun (p, n, _) -> (p, n))
  let occlusion =
    withProgressTxt len <| sprintf "Processing %i pixels"
    <| (fun tick ->
      depths
      |> mapi2d (fun x y (point, normalV) ->
        tick ()
        let samples = depths |> subGrid x y 5 |> Array.concat
        let o = occlusionAt point normalV samples |> float |> (*) 0.5
        add black (Color.scale o white)
      )
    )

  map22d subtract pixels occlusion

let aaOffsets offset =
  [ (0.f, offset); (0.f, -offset)
    (offset, 0.f); (-offset, 0.f) ]

let renderAA (c: Camera) w =
  let canv = canvas c.hSize c.vSize
  let len = 4 * Canvas.length canv

  withProgress len <| (fun tick ->
    canv |> Canvas.render (fun x y ->
      let (rs, gs, bs) =
        aaOffsets 0.35f
        |> List.map (fun (dx, dy) ->
          tick ()
          rayForPixel32 (float32 x + dx) (float32 y + dy) c
          |> colorAt w <| 4
          |> (fun c -> c.Return) )
        |> List.unzip3
      color <| List.average rs <| List.average gs <| List.average bs
    )
  )

let renderSection (fromX, fromY) (toX, toY) c w =
  let len = (toX - fromX) * (toY - fromY)
  let canv = canvas c.hSize c.vSize

  withProgress len <| (fun tick ->
    canv |> Canvas.render (fun x y ->
      let inRangeX = x >= fromX && x <= toX
      let inRangeY = y >= fromY && y <= toY
      if inRangeX && inRangeY
      then tick (); rayForPixel x y c |> colorAt w <| 4
      else white
    )
  )

let renderQuad n c w =
  let maxX = c.hSize - 1
  let maxY = c.vSize - 1
  let midX = 0.5 * float c.hSize |> Math.Floor |> int
  let midY = 0.5 * float c.vSize |> Math.Floor |> int
  [ ((0, 0), (midX, midY))
    ((midX + 1, 0), (maxX, midY))
    ((0, midY + 1), (midX, maxY))
    ((midX + 1, midY + 1), (maxX, maxY)) ]
  |> List.item n
  |> fun (from, To) -> renderSection from To c w
