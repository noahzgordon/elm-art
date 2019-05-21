module Clouds.EffectView exposing (draw)

import Arc2d
import Clouds.Model exposing (..)
import Color exposing (Color, rgb)
import Html exposing (Html)
import Html.Attributes exposing (id)
import Messages exposing (Message)
import Point2d as Point
import Svg.Attributes
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
        [ defs []
            [ linearGradient
                [ id "gradient"
                , gradientTransform [ Rotate 90 0 0 ]
                ]
                [ stop [ offset "0%", stopColor "#7593ab" ] []
                , stop [ offset "15%", stopColor "#cbc3c1" ] []
                , stop [ offset "25%", stopColor "#cbc3c1" ] []
                , stop [ offset "30%", stopColor "#cab0ac" ] []
                , stop [ offset "35%", stopColor "#8ea5ba" ] []
                , stop [ offset "65%", stopColor "#afbdc6" ] []
                , stop [ offset "100%", stopColor "#001a66" ] []
                ]
            ]
        , rect
            [ width (100 |> percent)
            , height (100 |> percent)
            , Svg.Attributes.fill "url(#gradient)"
            ]
            []
        , svg
            [ x (px 0)
            , y (px 260)
            ]
            (List.map drawCloudRow model.cloudRows)
        ]


drawCloudRow : CloudRow -> Svg Message
drawCloudRow row =
    svg
        [ x (px 0)
        , y (px (row.y - 10))
        , transform
            [ Scale row.xScale 1 ]
        , class [ "cloud-row" ]
        , opacity (Opacity row.opacity)
        ]
        [ g
            [ transform
                [ Scale 1 row.yScale ]
            ]
            (List.map (drawCloud row.height) row.clouds)
        ]


drawCloud : Float -> Cloud -> Svg Message
drawCloud rowHeight cloud =
    svg
        [ height (px rowHeight)
        , width (px cloud.width)
        , x (px cloud.x)
        ]
        [ TypedSvg.path
            [ d <|
                ("M " ++ String.fromFloat (cloud.width / 2 * cloud.corners.topLeft.horizontalLength) ++ ",0")
                    ++ (" H " ++ String.fromFloat (cloud.width - (cloud.width / 2 * cloud.corners.topRight.horizontalLength)))
                    ++ " "
                    ++ curve
                        ( cloud.width - (cloud.width / 2 * cloud.corners.topRight.horizontalLength * cloud.corners.topRight.firstControlPointX)
                        , 0
                        )
                        ( cloud.width
                        , rowHeight / 2 * cloud.corners.topRight.secondControlPointY
                        )
                        ( cloud.width
                        , rowHeight / 2
                        )
                    ++ curve
                        ( cloud.width
                        , rowHeight / 2 + (rowHeight / 2 * cloud.corners.bottomRight.secondControlPointY)
                        )
                        ( cloud.width - (cloud.width / 2 * cloud.corners.bottomRight.horizontalLength * cloud.corners.bottomRight.firstControlPointX)
                        , rowHeight
                        )
                        ( cloud.width - (cloud.width / 2 * cloud.corners.bottomRight.horizontalLength)
                        , rowHeight
                        )
                    ++ " H "
                    ++ String.fromFloat (cloud.width / 2 * cloud.corners.bottomLeft.horizontalLength)
                    ++ curve
                        ( cloud.width / 2 * cloud.corners.bottomLeft.horizontalLength * cloud.corners.bottomLeft.firstControlPointX
                        , rowHeight
                        )
                        ( 0
                        , rowHeight - (rowHeight / 2 * cloud.corners.bottomLeft.secondControlPointY)
                        )
                        ( 0
                        , rowHeight / 2
                        )
                    ++ curve
                        ( 0
                        , rowHeight / 2 * cloud.corners.topLeft.secondControlPointY
                        )
                        ( cloud.width / 2 * cloud.corners.topLeft.horizontalLength * cloud.corners.topLeft.firstControlPointX
                        , 0
                        )
                        ( cloud.width / 2 * cloud.corners.topLeft.horizontalLength
                        , 0
                        )
                    ++ " Z"
            , fill (Fill Color.white)
            ]
            []
        ]


curve : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> String
curve ( x1, y1 ) ( x2, y2 ) ( x, y ) =
    "C "
        ++ String.fromFloat x1
        ++ " "
        ++ String.fromFloat y1
        ++ ","
        ++ String.fromFloat x2
        ++ " "
        ++ String.fromFloat y2
        ++ ","
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y
