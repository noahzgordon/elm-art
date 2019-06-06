module Noise.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Noise.Model exposing (..)
import Perlin
import TypedSvg exposing (..)
import TypedSvg.Attributes as Attributes exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)


draw : Model -> Html Message
draw model =
    let
        imageWidth =
            model.window.width - 200

        baseHeight =
            model.window.height / 2
    in
    svg
        [ width (imageWidth |> px)
        , height (model.window.height |> px)
        ]
        (List.concat
            [ [ line
                    [ x1 (px 20)
                    , x2 (px <| imageWidth - 20)
                    , y1 (px baseHeight)
                    , y2 (px baseHeight)
                    , stroke (rgba 0 0 0 0.3)
                    , strokeWidth (px 5)
                    ]
                    []
              ]
            , List.range 20 (round <| imageWidth - 20)
                |> List.map toFloat
                |> List.foldl
                    (\x ( lastY, lines ) ->
                        let
                            xPos =
                                x / imageWidth - 40

                            newY =
                                Perlin.noise ( xPos, lastY, toFloat model.time / 8640 ) model.seed
                        in
                        ( newY
                        , lines
                            ++ [ line
                                    [ x1 (px x)
                                    , x2 (px (x + 1))
                                    , y1 (px (lastY * model.window.height))
                                    , y2 (px (newY * model.window.height))
                                    , stroke (rgba 0 0 0 1)
                                    ]
                                    []
                               ]
                        )
                    )
                    ( Perlin.noise ( 19 / imageWidth - 40, 0, toFloat model.time / 8640 ) model.seed, [] )
                |> Tuple.second
            ]
        )
