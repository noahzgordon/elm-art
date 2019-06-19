module Sutcliffe.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Html exposing (Html)
import Messages exposing (Message)
import Perlin
import Point2d exposing (xCoordinate, yCoordinate)
import Random
import Sutcliffe.Model exposing (..)
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
        [ g
            [ transform
                [ Scale model.scale model.scale
                , Rotate model.rotation 0 0
                ]
            , class [ "transform-center" ]
            ]
          <|
            List.map drawPent model.finished
                ++ [ drawPent model.growing ]
        ]


drawPent : Pent -> Svg Message
drawPent pent =
    g [] <|
        List.concat
            [ List.map drawLine pent.sides
            , List.map drawLine pent.struts
            ]


drawLine : Line -> Svg Message
drawLine lineData =
    let
        endPoint =
            Point2d.interpolateFrom lineData.origin lineData.endpoint lineData.growth
    in
    line
        [ x1 (px <| xCoordinate lineData.origin)
        , y1 (px <| yCoordinate lineData.origin)
        , x2 (px <| xCoordinate endPoint)
        , y2 (px <| yCoordinate endPoint)
        , stroke Color.black
        , strokeWidth (px 2)
        ]
        []
