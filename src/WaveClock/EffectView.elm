module WaveClock.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Perlin
import Random
import Time exposing (utc)
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)
import WaveClock.Model exposing (..)


draw : Model -> Html Message
draw model =
    let
        imageWidth =
            model.window.width - 200

        nothing =
            model.modifiers.radNoise
    in
    svg
        [ width (imageWidth |> px)
        , height (model.window.height |> px)
        ]
        (List.map
            (\data ->
                line
                    [ x1 (px data.x1)
                    , y1 (px data.y1)
                    , x2 (px data.x2)
                    , y2 (px data.y2)
                    , stroke data.color
                    ]
                    []
            )
            model.lines
        )
