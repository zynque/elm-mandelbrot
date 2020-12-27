port module MyCanvas exposing (..)

import Json.Encode as E


port writeImage : E.Value -> Cmd msg

type alias RawCanvasImageData = List Int

buildWriteImageCommand : Int -> Int -> RawCanvasImageData -> Cmd msg
buildWriteImageCommand width height imageData =
  let data = E.list E.int imageData
      message = E.object
        [ ("x", E.int 5)
        , ("y", E.int 5)
        , ("width", E.int width)
        , ("height", E.int height)
        , ("data", data)
        ]
  in writeImage message
