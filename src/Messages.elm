module Messages exposing (CloudModifier(..), Effect(..), LightningModifier(..), Message(..), MetaEffect(..), Modifier(..))

import Clouds.Model
import Html exposing (Html)
import Json.Decode as Json
import Lightning.Model
import Noise.Model
import NoiseOverTime.Model
import Time exposing (Posix)


type Effect model mod
    = Effect
        { id : String
        , name : String
        , draw : model -> Html Message
        , mods : List ( mod, String, model -> Float )
        , model : model
        , tick : Posix -> model -> model
        , modConstructor : mod -> Modifier
        , applyModifier : Effect model mod -> mod -> Float -> Effect model mod
        }


type MetaEffect
    = CloudEffect (Effect Clouds.Model.Model CloudModifier)
    | LightningEffect (Effect Lightning.Model.Model LightningModifier)
    | NoiseEffect (Effect Noise.Model.Model ())
    | NoiseOverTimeEffect (Effect NoiseOverTime.Model.Model ())


type Modifier
    = CloudMod CloudModifier
    | LightningMod LightningModifier
    | NoiseMod ()


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
    | UserSelectedEffect MetaEffect
