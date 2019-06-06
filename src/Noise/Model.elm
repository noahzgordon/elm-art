module Noise.Model exposing (Model, init)

import Random


type alias Model =
    { window : Dimensions
    , seed : Random.Seed
    , time : Int
    }


type alias Dimensions =
    { width : Float, height : Float }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    { window = flags.window
    , seed = Random.initialSeed flags.time
    , time = flags.time
    }
