module Messages exposing (Message(..), Modifier(..))

import Json.Decode as Json
import Time exposing (Posix)


type Modifier
    = Extremity


type Message
    = AnimationFrameTriggered Posix
    | ModifierChanged Modifier Float
    | MidiInputReceived Json.Value
