module Fractals exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Bitwise
import Mandelbrot exposing (..)
import MyCanvas exposing (..)


type alias RGBA = {r: Int, g: Int, b: Int, a: Int}

type alias CanvasImageDataPoints = List RGBA

type alias Point a = (a, a)

type alias Rect a = (Point a, Point a)

type alias PointF = Point Float

type alias RectF = Rect Float

windowWidth = 700
windowHeight = 450
windowXs = List.range 0 (windowWidth - 1)
windowYs = List.range 0 (windowHeight - 1)
windowGrid = List.map (\_ -> windowXs) windowYs

windowRect : RectF
windowRect = ((0.0,0.0),(toFloat windowWidth, toFloat windowHeight))

aspectRatio = (toFloat windowHeight) / (toFloat windowWidth)

widthOf rectangle =
  let ((x1, _), (x2, _)) = rectangle
  in x2 - x1

heightOf rectangle =
  let ((_, y1), (_, y2)) = rectangle
  in y2 - y1

aspectRatioOf rectangle =
  (widthOf rectangle) / (heightOf rectangle)

mandlebrotBounds = ((-2.2,-1.2), (1.2, 1.2))

fitRectangle : RectF -> RectF -> RectF
fitRectangle container target =
  let ((tx1,ty1),(tx2,ty2)) = target
      car = aspectRatioOf container
      tar = aspectRatioOf target
  in
    if car > tar then
      -- container wider, fill horizontally
      let desiredWidth =  heightOf target * car
          halfFillWidth = (desiredWidth - widthOf target) / 2.0
      in  ((tx1 - halfFillWidth, ty1), (tx2 + halfFillWidth, ty2))
    else
      -- container higher, fill vertically
      let desiredHeight =  widthOf target / car
          halfFillHeight = (desiredHeight - heightOf target) / 2.0
      in  ((tx1, ty1 - halfFillHeight), (tx2, ty2 + halfFillHeight))

unzoomedWorldRectangle = fitRectangle windowRect mandlebrotBounds

colorizef a = {
    r = modBy 255 (round ((sin (10.0*a + (1.0*pi/8.0)))*254.0))
    , g = remainderBy 255 (round ((sin (10.0*a + (2.0*pi/8.0))) * 254.0))
    , b = remainderBy 255 (round ((sin (10.0*a + (3.0*pi/8.0))) * 254.0))
    , a = 255
    }

mandelColor (mag, iters) = if (mag < 4.0) then 1.0 else 1.0 - (1.0 / (0.3 * (toFloat iters)))

colorFunc : PointF -> RGBA
colorFunc (x, y) = colorizef (mandelColor (mandel (x, y)))

scalePoint : RectF -> RectF -> PointF -> PointF 
scalePoint sourceRect targetRect point =
  let (x, y) = point
      ((sx1,sy1),_) = sourceRect
      ((tx1,ty1),_) = targetRect
      sourceWidth = widthOf sourceRect
      targetWidth = widthOf targetRect
      scaleFactor = targetWidth / sourceWidth
      sourceOffsetX = x - sx1
      sourceOffsetY = y - sy1
      targetOffsetX = sourceOffsetX * scaleFactor
      targetOffsetY = sourceOffsetY * scaleFactor
      targetX = tx1 + targetOffsetX
      targetY = ty1 + targetOffsetY
  in (targetX, targetY)

scaleWindowToWorld : PointF -> PointF
scaleWindowToWorld = scalePoint windowRect unzoomedWorldRectangle

windowToWorldCoordinates: Point Int -> PointF
windowToWorldCoordinates (xi, yi) =
  let pointF = (toFloat xi, toFloat yi)
  in scaleWindowToWorld pointF

zoomCoordinates: Float -> PointF -> PointF -> PointF
zoomCoordinates zoomFactor center point =
  let (px, py) = point
      (xOffset, yOffset) = center
      x = px / zoomFactor + xOffset
      y = py / zoomFactor + yOffset
  in  (x, y)

zoom: PointF -> PointF
-- zoom = zoomCoordinates 80 (0.35, 0.580006)
zoom = zoomCoordinates 1 (0.0, 0.0)

calcRow y row = List.map (\x -> colorFunc (zoom (windowToWorldCoordinates (x, y)))) row

coloredGrid = List.indexedMap calcRow windowGrid

flattenedGrid = List.concat coloredGrid

exampleImage = flattenedGrid

canvasDataToRaw : CanvasImageDataPoints -> RawCanvasImageData
canvasDataToRaw points = List.reverse(List.foldl accumulateImageDataPoint [] points)

accumulateImageDataPoint {r,g,b,a} accumulator = List.append [a,b,g,r] accumulator

raw = canvasDataToRaw exampleImage

type alias Model = RectF

type Msg
    = Zoom RectF
    | Write RawCanvasImageData

main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> ( Model, Cmd Msg )
init () =
    ( ((-1,-1), (1,1)), buildWriteImageCommand windowWidth windowHeight raw )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = (model, Cmd.none)

printRect rect =
  let ((x1,y1),(x2,y2)) = rect
      ff = String.fromFloat
  in "(" ++ ff x1 ++ ", " ++ ff y1 ++ "), (" ++ ff x2 ++ ", " ++ ff y2 ++ ")"

diagnostic = 
  let wr = unzoomedWorldRectangle
      p1 = scalePoint windowRect wr (1.0,1.0)
      p2 = scalePoint windowRect wr (toFloat windowWidth - 1.0, toFloat windowHeight - 1.0) 
  in printRect windowRect ++ "-->" ++ printRect unzoomedWorldRectangle ++ "=" ++ printRect (p1,p2)

view : Model -> Html Msg
view ((x1,y1),(x2,y2)) =
    text diagnostic
