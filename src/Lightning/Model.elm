module Lightning.Model exposing (Arc(..), Bolt, Coords, Dimensions, Model, init)

import Random


type alias Model =
    { window : Dimensions
    , bolts : List Bolt
    , fremulation : Float
    , chaos : Float
    , dilation : Float
    , zoom : Float
    }


type alias Dimensions =
    { width : Float, height : Float }


type alias Coords =
    { x : Float, y : Float }


type alias Bolt =
    { origin : Coords
    , lifeTime : Int
    , seed : Random.Seed
    , arcs : List Arc
    }


type Arc
    = Arc ArcInfo


type alias ArcInfo =
    { length : Float
    , arcs : List Arc
    , angle : Float
    , origAngle : Float
    }


init : { a | window : Dimensions } -> Model
init flags =
    { window = flags.window
    , bolts = []
    , fremulation = 0.5
    , chaos = 0.5
    , dilation = 0.5
    , zoom = 0.5
    }
