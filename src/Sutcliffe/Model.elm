module Sutcliffe.Model exposing (Embellishment, Line, Model, Pent, Phase(..), StrutGroup, init, lineSegment, spawnGroups)

import Arc2d
import Color exposing (Color, rgba)
import Direction2d as Direction
import LineSegment2d as LineSegment exposing (LineSegment2d)
import List.Extra as List
import Point2d as Point exposing (Point2d)
import QuadraticSpline2d as QuadraticSpline exposing (QuadraticSpline2d)
import Random
import Time exposing (Posix)
import Vector2d as Vector


type alias Model =
    { window : Dimensions
    , time : Posix
    , strutLength : Float
    , growing : Pent
    , finished : List Pent
    , phase : Phase
    , scale : Float
    , rotation : Float
    , pentCount : Int
    }


type Phase
    = Struts
    | Sides


type alias Embellishment =
    { first : QuadraticSpline2d
    , second : QuadraticSpline2d
    , third : QuadraticSpline2d
    , growth : Float
    }


type alias StrutGroup =
    { strut : Line
    , sides : ( Line, Line )
    , embellishments : ( Embellishment, Embellishment )
    }


type alias Pent =
    { groups : List StrutGroup
    , color : Color
    , pentNum : Int
    }


type alias Line =
    { origin : Point2d
    , endpoint : Point2d
    , growth : Float
    }


type alias Dimensions =
    { width : Float, height : Float }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    let
        centerPoint =
            Point.fromCoordinates ( (flags.window.width - 200) / 2, flags.window.height / 2 )

        initialStruts =
            [ 0, 72, 144, 216, 288 ]
                |> List.map
                    (\angle ->
                        { origin = centerPoint
                        , endpoint =
                            Arc2d.with
                                { centerPoint = centerPoint
                                , radius = 10
                                , startAngle = 0
                                , sweptAngle = degrees angle
                                }
                                |> Arc2d.endPoint
                        , growth = 0
                        }
                    )
    in
    { window = flags.window
    , time = Time.millisToPosix flags.time
    , strutLength = 10
    , growing =
        { groups = spawnGroups initialStruts
        , color = rgba 0.5 0.5 0.5 0.5
        , pentNum = 0
        }
    , finished = []
    , phase = Struts
    , scale = 1
    , rotation = 0
    , pentCount = 0
    }


spawnGroups : List Line -> List StrutGroup
spawnGroups lines =
    let
        triples =
            List.zip3 lines (shift lines) ((shift >> shift) lines)
    in
    List.map
        (\( a, b, c ) ->
            let
                ( lineA, lineB, lineC ) =
                    ( lineSegment a, lineSegment b, lineSegment c )

                sideA =
                    { origin = LineSegment.endPoint lineB
                    , endpoint = Point.midpoint (LineSegment.endPoint lineB) (LineSegment.endPoint lineA)
                    , growth = 0
                    }

                sideC =
                    { origin = LineSegment.endPoint lineB
                    , endpoint = Point.midpoint (LineSegment.endPoint lineB) (LineSegment.endPoint lineC)
                    , growth = 0
                    }
            in
            { strut = b
            , sides = ( sideA, sideC )
            , embellishments = spawnEmbellishments b ( sideA, sideC )
            }
        )
        triples


spawnEmbellishments : Line -> ( Line, Line ) -> ( Embellishment, Embellishment )
spawnEmbellishments strut sides =
    let
        spawn : LineSegment2d -> LineSegment2d -> Embellishment
        spawn strutSegment sideSegment =
            let
                outsideLine =
                    LineSegment.translateBy (LineSegment.vector sideSegment) strutSegment

                bottomLine =
                    LineSegment.translateBy (Vector.reverse <| LineSegment.vector strutSegment) sideSegment
            in
            { first =
                QuadraticSpline.with
                    { startPoint = LineSegment.midpoint strutSegment
                    , controlPoint = LineSegment.endPoint strutSegment
                    , endPoint = LineSegment.midpoint sideSegment
                    }
            , second =
                QuadraticSpline.with
                    { startPoint = LineSegment.midpoint sideSegment
                    , controlPoint = LineSegment.endPoint sideSegment
                    , endPoint = LineSegment.midpoint outsideLine
                    }
            , third =
                QuadraticSpline.with
                    { startPoint = LineSegment.midpoint outsideLine
                    , controlPoint = LineSegment.endPoint outsideLine
                    , endPoint = LineSegment.midpoint bottomLine
                    }
            , growth = 0
            }
    in
    ( spawn (lineSegment strut) (lineSegment <| Tuple.first sides)
    , spawn (lineSegment strut) (lineSegment <| Tuple.second sides)
    )


lineSegment : Line -> LineSegment2d
lineSegment line =
    LineSegment.from line.origin line.endpoint


shift : List a -> List a
shift list =
    Maybe.map2
        (\head tail ->
            tail ++ [ head ]
        )
        (List.head list)
        (List.tail list)
        |> Maybe.withDefault list
