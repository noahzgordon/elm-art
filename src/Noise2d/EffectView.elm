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
                    , x2 (px (lineData.x + 40))
                    , y1 (px lineData.y)
                    , y2 (px lineData.y)
                    , stroke (rgba lineData.noise 0.7 (1 - lineData.noise) lineData.noise)
                    , strokeWidth (px 4)
                    , transform
                        [ Rotate (lineData.noise * radians 360) lineData.x lineData.y ]
                    ]
                    []
            )
            model.lines
