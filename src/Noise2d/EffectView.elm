module Noise2d.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Noise2d.Model exposing (..)
import Random
import Time exposing (utc)
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


draw : Model -> Html Message
draw model =
    svg
        [ width (model.window.width - 200 |> px)
        , height (model.window.height |> px)
        ]
    <|
        List.map
            (\lineData ->
                line
                    [ x1 (px lineData.x)
                    , x2 (px (lineData.x + 20))
                    , y1 (px lineData.y)
                    , y2 (px lineData.y)
                    , stroke (rgba 0 0 0 1)
                    , strokeWidth (px 1)
                    , transform
                        [ Rotate lineData.rotation lineData.x lineData.y ]
                    , class [ "center-transform" ]
                    ]
                    []
            )
            model.lines
