module Sutcliffe.Model exposing (Model, init)

import Color exposing (Color)
import Random
import Time exposing (Posix)


type alias Model =
    { window : Dimensions
    , time : Posix
    }


type alias Dimensions =
    { width : Float, height : Float }


init : { a | window : Dimensions, time : Int } -> Model
init flags =
    { window = flags.window
    , time = Time.millisToPosix flags.time
    }
