port module Interop exposing (midiInput)

import Json.Decode as Json


port midiInput : (Json.Value -> msg) -> Sub msg
