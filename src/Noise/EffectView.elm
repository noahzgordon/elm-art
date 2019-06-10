module Noise.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Noise.Model exposing (..)
import Perlin
import Random
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

        xStep =
            10

        amplitude =
            200

        ( xOffsets, _ ) =
            Random.step
                ((model.window.width - 240)
                    / xStep
                    |> (\numGroups -> Random.list (round numGroups) (Random.float 0 xStep))
                )
                model.seed

        xPositions =
            List.foldl
                (\offset ( lastPosStart, positions ) ->
                    ( lastPosStart + xStep
                    , positions
                        ++ [ lastPosStart + offset ]
                    )
                )
                ( 20, [] )
                xOffsets
                |> Tuple.second
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
            , List.foldl
                (\x ( lastX, lastY, lines ) ->
                    let
                        newY =
                            Perlin.noise ( x, 0, 0 ) model.seed
                                * amplitude
                                + (model.window.height / 2)
                                - (amplitude / 2)
                    in
                    ( x
                    , newY
                    , lines
                        ++ [ line
                                [ x1 (px lastX)
                                , x2 (px x)
                                , y1 (px lastY)
                                , y2 (px newY)
                                , stroke (rgba 0 0 0 1)
                                ]
                                []
                           ]
                    )
                )
                ( 20
                , Perlin.noise ( 20, 0, 0 ) model.seed * amplitude + (model.window.height / 2) - (amplitude / 2)
                , []
                )
                xPositions
                |> (\( _, _, third ) -> third)
            ]
        )
