module WaveClock.Model exposing (Model, init)

import Color exposing (Color)
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , seed : Random.Seed
    , time : Posix
    , angNoise : Float
    , radNoise : Float
    , xNoise : Float
    , yNoise : Float
    , angle : Float
    , lines : List Line
    , modifiers :
        { radNoise : Float
        , angNoise : Float
        , radius : Float
        , step : Float
        , delay : Float
        , hue : Float
        , saturation : Float
        , lightness : Float
        }
    , lastTick : Int
    }


type alias Line =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , color : Color
    }


type alias Dimensions =
    { width : Float, height : Float }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    let
        seed0 =
            Random.initialSeed flags.time

        ( angNoise, seed1 ) =
            Random.step (Random.float 0 10) seed0

        ( radiusNoise, seed2 ) =
            Random.step (Random.float 0 10) seed1

        ( xNoise, seed3 ) =
            Random.step (Random.float 0 10) seed2

        ( yNoise, seed4 ) =
            Random.step (Random.float 0 10) seed3
    in
    { window = flags.window
    , seed = seed4
    , time = Time.millisToPosix flags.time
    , angNoise = angNoise
    , radNoise = radiusNoise
    , xNoise = xNoise
    , yNoise = yNoise
    , angle = -pi / 2
    , lines = []
    , modifiers =
        { angNoise = 1
        , radNoise = 1
        , radius = 1
        , step = 1
        , delay = 0
        , hue = 0
        , saturation = 0
        , lightness = 0.5
        }
    , lastTick = flags.time
    }
