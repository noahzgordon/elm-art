module WaveClock.Model exposing (Model, init)

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
    }
