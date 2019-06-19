module Sutcliffe.Model exposing (Line, Model, Pent, Phase(..), init)

import Arc2d
import Color exposing (Color)
import Point2d exposing (Point2d)
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , time : Posix
    , strutLength : Float
    , growing : Pent
    , finished : List Pent
    , phase : Phase
    , scale : Float
    , rotation : Float
    }


type Phase
    = Struts
    | Sides


type alias Pent =
    { sides : List Line
    , struts : List Line
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
            Point2d.fromCoordinates ( (flags.window.width - 200) / 2, flags.window.height / 2 )

        growing =
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
    , growing = { struts = growing, sides = [] }
    , finished = []
    , phase = Struts
    , scale = 1
    , rotation = 0
    }
