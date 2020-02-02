module rec Camera

open System
open System.Numerics
open ShellProgressBar

open Blur
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

let outputFile path canvas =
  let path =
    match path with
    | None -> ("../render/" + (Util.nowStr ()) + ".ppm")
    | Some path -> path
  Util.writeFile path (Canvas.toPpm canvas)

let withProgress len txt fn =
  let bar = new ProgressBar (len, txt, ConsoleColor.Yellow)
  let result = fn (fun _ -> bar.Tick ())
  printfn "\n" // To avoid CLI glitch after rendering
  result

type SectionType =
  | Quad of int
  | Section of int * int * int * int

type RenderOptions = {
  antiAliasing: bool
  path: string option
  section: SectionType option
}

let defaultOptions = {
  antiAliasing = false
  path = None
  section = None
}

let render options camera world =
  outputFile options.path (renderImage options camera world)

let renderFX options camera world fn =
  let image =
    renderImage options camera world
  outputFile options.path (fn image)
  

let renderImage o c w =
  let canv = canvas c.hSize c.vSize
  let len = Canvas.length canv

  withProgress len (sprintf "Rendering %i pixels" len) (fun tick ->
    canv |> Canvas.mapi (fun x y _ ->
      tick ()
      match shouldRender o.section x y c with
      | true ->
        if o.antiAliasing then renderAA x y c w colorAndAmbientAt
        else rayForPixel x y c |> colorAndAmbientAt w <| 4
      | false -> white
    )
  )

let rayForPixel32 x y c =
  let xOffset = (x + 0.5f) * c.pixelSize
  let yOffset = (y + 0.5f) * c.pixelSize
  let worldX = c.halfWidth - xOffset
  let worldY = c.halfHeight - yOffset
  let pixel = transform (inverse c.transform) (point32 worldX worldY -1.f)
  let origin = transform (inverse c.transform) (point32 0.f 0.f 0.f)
  let direction = pixel - origin |> normalize
  ray origin direction

let rayForPixel x y c =
  rayForPixel32 (float32 x) (float32 y) c

let aaOffsets offset =
  [| (0.f, offset); (0.f, -offset);
    (offset, 0.f); (-offset, 0.f) |]

let renderAA x y c w renderFn =
  aaOffsets 0.35f
  |> Array.map (fun (dx, dy) ->
    rayForPixel32 (float32 x + dx) (float32 y + dy) c
    |> renderFn w <| 4
    |> toRGB
  ) |> Color.average

let renderDepth minDepth maxDepth c w =
  let canv = canvas c.hSize c.vSize
  withProgress (length canv) "Rendering depth" <| (fun tick ->
    canv |> Canvas.mapi (fun x y _ ->
      tick ()
      match rayForPixel x y c |> depthAt w with
      | Some d ->
        float d |> clamp minDepth maxDepth
        |> rangeMap (minDepth, maxDepth) (1., 0.) |> flip Color.scale white
      | None -> black
    )
  )

let applyOcclusion opacity color image occlusion =
  map2d2Parallel (fun imageColor ao ->
    let mixAmt = rangeMap (0., 1.) (1., (1. - opacity)) ao
    mix mixAmt color imageColor
  ) image occlusion

let renderOcclusion options samples threshold c w =
  let canv = canvas c.hSize c.vSize
  let len = 3 * Canvas.length canv

  withProgress len "Rendering AO" <| (fun tick ->
    canv |> Canvas.mapi (fun x y _ ->
      tick ()
      match shouldRender options.section x y c with
      | true ->
        occlusionAt samples threshold w
        <| rayForPixel x y c
      | false -> 0.
    )
    |> bilateralFilter 8 50. 0.15 tick
    |> bilateralFilter 3 10. 0.15 tick
  )

let shouldRender section x y c =
  match section with
  | None -> true
  | Some (Section (fromX, fromY, toX, toY)) ->
    let inRangeX = x >= fromX && x <= toX
    let inRangeY = y >= fromY && y <= toY
    inRangeX && inRangeY

  | Some (Quad n) ->
    let maxX = c.hSize - 1
    let maxY = c.vSize - 1
    let midX = 0.5 * float c.hSize |> Math.Floor |> int
    let midY = 0.5 * float c.vSize |> Math.Floor |> int
    [ (0, 0, midX, midY)
      (midX + 1, 0, maxX, midY)
      (0, midY + 1, midX, maxY)
      (midX + 1, midY + 1, maxX, maxY) ]
    |> List.item n
    |> fun range -> shouldRender (Some <| Section range) x y c
