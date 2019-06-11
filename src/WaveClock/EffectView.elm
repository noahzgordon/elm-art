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

        initAngle =
            -pi * 2

        numIterations =
            10000

        baseAngleStep =
            6

        iterate num accum =
            let
                radNoise =
                    accum.angNoise + 0.005

                radius =
                    (Perlin.noise ( radNoise, 0, 0 ) model.seed * 550) + 1

                angNoise =
                    accum.angNoise + 0.005

                baseAngle =
                    accum.angle
                        + (Perlin.noise ( accum.angNoise, 0, 0 ) model.seed * baseAngleStep)
                        - 3

                angle =
                    if baseAngle > 360 then
                        baseAngle - 360

                    else if baseAngle < 0 then
                        baseAngle + 360

                    else
                        baseAngle

                ( xNoise, yNoise ) =
                    ( accum.xNoise + 0.01
                    , accum.yNoise + 0.01
                    )

                ( centerX, centerY ) =
                    ( imageWidth / 2 + (Perlin.noise ( xNoise, 0, 0 ) model.seed * 100) - 50
                    , model.window.height / 2 + (Perlin.noise ( yNoise, 0, 0 ) model.seed * 100) - 50
                    )

                rad =
                    radians angle

                oppRad =
                    rad + pi

                colorVal =
                    accum.colorVal + accum.colorChange
            in
            { angle = angle
            , angNoise = angNoise
            , radNoise = radNoise
            , xNoise = xNoise
            , yNoise = yNoise
            , colorVal = colorVal
            , colorChange =
                if colorVal > 254 then
                    -1

                else if colorVal < 0 then
                    1

                else
                    accum.colorChange
            , lines =
                accum.lines
                    ++ [ { x1 = centerX + (radius * cos rad)
                         , y1 = centerY + (radius * sin rad)
                         , x2 = centerX + (radius * cos oppRad)
                         , y2 = centerY + (radius * sin oppRad)
                         , color = rgba (colorVal / 255) (colorVal / 255) (colorVal / 255) (60 / 255)
                         }
                       ]
            }

        iterations =
            List.range 0 numIterations
                |> List.foldl iterate
                    { lines = []
                    , angNoise = model.angNoise
                    , radNoise = model.radNoise
                    , xNoise = model.xNoise
                    , yNoise = model.yNoise
                    , angle = -pi / 2
                    , colorVal = 254
                    , colorChange = -1
                    }
                |> .lines
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
            iterations
        )
