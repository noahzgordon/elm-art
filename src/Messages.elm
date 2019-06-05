module Messages exposing (CloudModifier(..), LightningModifier(..), Message(..), Modifier(..))

import Json.Decode as Json
import Time exposing (Posix)


type Modifier
    = CloudMod CloudModifier
    | LightningMod LightningModifier


type CloudModifier
    = Extremity
    | Speed


type LightningModifier
    = Fremulation
    | Chaos
    | Dilation
    | Zoom


type Message
    = AnimationFrameTriggered Posix
    | ModifierChanged Modifier Float
    | MidiInputReceived Json.Value
