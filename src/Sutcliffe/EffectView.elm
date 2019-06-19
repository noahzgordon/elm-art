module Sutcliffe.EffectView exposing (draw)

import Color exposing (Color, rgba)
import Geometry.Svg
import Html exposing (Html)
import Messages exposing (Message)
import Perlin
import Point2d exposing (xCoordinate, yCoordinate)
import Polyline2d
import Random
import Sutcliffe.Model exposing (..)
import Svg.Keyed
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
        [ Svg.Keyed.node "g"
            [ transform
                [ Scale model.scale model.scale
                , Rotate model.rotation 0 0
                ]
            , class [ "transform-center" ]
            ]
          <|
            List.concat
                [ [ drawPent False model.scale model.growing ]
                , List.map (drawPent True model.scale) model.finished
                ]
        ]


drawPent : Bool -> Float -> Pent -> ( String, Svg Message )
drawPent isFinished scale pent =
    ( String.fromInt pent.pentNum
    , g
        [ stroke Color.black
        , strokeWidth (px (1 / scale))
        ]
      <|
        List.concat
            [ [ List.map (\group -> group.strut.endpoint) pent.groups
                    |> Polyline2d.fromVertices
                    |> Geometry.Svg.polyline2d
                        [ fill (Fill pent.color)
                        , class
                            (if isFinished then
                                [ "pent", "revealed" ]

                             else
                                [ "pent" ]
                            )
                        ]
              ]
            , List.map (drawGroup scale) pent.groups
            ]
    )


drawGroup : Float -> StrutGroup -> Svg Message
drawGroup scale group =
    g []
        [ drawLine scale group.strut
        , drawLine scale (Tuple.first group.sides)
        , drawLine scale (Tuple.second group.sides)
        , drawEmbellishment (Tuple.first group.embellishments)
        , drawEmbellishment (Tuple.second group.embellishments)
        ]


drawLine : Float -> Line -> Svg Message
drawLine scale lineData =
    let
        endPoint =
            Point2d.interpolateFrom lineData.origin lineData.endpoint lineData.growth
    in
    line
        [ x1 (px <| xCoordinate lineData.origin)
        , y1 (px <| yCoordinate lineData.origin)
        , x2 (px <| xCoordinate endPoint)
        , y2 (px <| yCoordinate endPoint)
        ]
        []


drawEmbellishment : Embellishment -> Svg Message
drawEmbellishment embellishment =
    g [] []
