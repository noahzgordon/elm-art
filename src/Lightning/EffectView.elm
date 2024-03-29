module Lightning.EffectView exposing (draw)

import Arc2d
import Color exposing (Color, rgb)
import Html exposing (Html)
import Lightning.Model exposing (..)
import Messages exposing (Message)
import Point2d as Point
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
        ([ rect
            [ width (100 |> percent)
            , height (100 |> percent)
            , fill (rgb 0 0 0 |> Fill)
            ]
            []
         ]
            ++ List.concat (List.map drawBolt model.bolts)
        )


drawBolt : Bolt -> List (Svg Message)
drawBolt bolt =
    List.concat (List.map (drawArc bolt.origin) bolt.arcs)


drawArc : Coords -> Arc -> List (Svg Message)
drawArc { x, y } (Arc arc) =
    let
        ( endX, endY ) =
            Arc2d.with
                { centerPoint = Point.fromCoordinates ( x, y )
                , radius = arc.length
                , startAngle = 0
                , sweptAngle = arc.angle
                }
                |> Arc2d.endPoint
                |> Point.coordinates
    in
    [ line
        [ x1 (x |> px)
        , y1 (y |> px)
        , x2 (endX |> px)
        , y2 (endY |> px)
        , stroke arcColor
        ]
        []
    ]
        ++ List.concat (List.map (drawArc { x = endX, y = endY }) arc.arcs)


arcColor : Color
arcColor =
    rgb 1 1 1
