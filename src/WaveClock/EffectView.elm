module WaveClock.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Noise.Model exposing (..)
import Perlin
import Random
import Time exposing (utc)
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


draw : Model -> Html Message
draw model =
    let
        imageWidth =
            model.window.width - 200
    in
    svg
        [ width (imageWidth |> px)
        , height (model.window.height |> px)
        ]
        []
