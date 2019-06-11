module WaveClock.Model exposing (Model, init)

import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , seed : Random.Seed
    , time : Posix
    }


type alias Dimensions =
    { width : Float, height : Float }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    { window = flags.window
    , seed = Random.initialSeed flags.time
    , time = Time.millisToPosix flags.time
    }
