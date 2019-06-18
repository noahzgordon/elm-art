module Model exposing (Model, init)

import Clouds.EffectView
import Clouds.Model
import Clouds.Update
import Effects
import Html exposing (Html)
import Lightning.EffectView
import Lightning.Model
import Lightning.Update
import Messages exposing (..)
import Noise.EffectView
import Noise.Model
import Noise2d.EffectView
import Noise2d.Model
import Noise2d.Update
import NoiseOverTime.EffectView
import NoiseOverTime.Model
import Sutcliffe.EffectView
import Sutcliffe.Model
import Sutcliffe.Update exposing (Modifier)
import Time exposing (Posix)
import WaveClock.EffectView
import WaveClock.Model
import WaveClock.Update exposing (Modifier(..))


type alias Model =
    { currentEffect : MetaEffect
    , otherEffects : List MetaEffect
    }


type alias Dimensions =
    { width : Float, height : Float }


type alias Flags =
    { window : Dimensions
    , time : Int
    }


init : Flags -> ( Model, Cmd Message )
init flags =
    ( { currentEffect =
            NoiseEffect <|
                Effects.build
                    { name = "Noise"
                    , id = "noise"
                    , draw = Noise.EffectView.draw
                    , mods = []
                    , model = Noise.Model.init flags
                    , tick = \t m -> { m | time = t }
                    , modConstructor = NoiseMod
                    , applyModifier = \eff _ _ -> eff
                    }
      , otherEffects =
            [ NoiseOverTimeEffect <|
                Effects.build
                    { name = "Noise Over Time"
                    , id = "noise-over-time"
                    , draw = NoiseOverTime.EffectView.draw
                    , mods = []
                    , model = NoiseOverTime.Model.init flags
                    , tick = \t m -> { m | time = t }
                    , modConstructor = NoiseMod
                    , applyModifier = \eff _ _ -> eff
                    }
            , Noise2dEffect <|
                Effects.build
                    { name = "Noise 2D"
                    , id = "noise-2d"
                    , draw = Noise2d.EffectView.draw
                    , mods = []
                    , model = Noise2d.Model.init flags
                    , tick = Noise2d.Update.tick
                    , modConstructor = NoiseMod
                    , applyModifier = \eff _ _ -> eff
                    }
            , LightningEffect <|
                Effects.build
                    { name = "Fork Lightning"
                    , id = "fork-lightning"
                    , draw = Lightning.EffectView.draw
                    , mods =
                        [ ( Fremulation, "fremulation", .fremulation )
                        , ( Chaos, "chaos quotient", .chaos )
                        , ( Dilation, "time dilation", .dilation )
                        , ( Zoom, "zoominess", .zoom )
                        ]
                    , model = Lightning.Model.init flags
                    , tick = Lightning.Update.tick
                    , modConstructor = LightningMod
                    , applyModifier =
                        \m mod val ->
                            case mod of
                                Fremulation ->
                                    { m | fremulation = val }

                                Chaos ->
                                    { m | chaos = val }

                                Dilation ->
                                    { m | dilation = val }

                                Zoom ->
                                    { m | zoom = val }
                    }
            , CloudEffect <|
                Effects.build
                    { name = "O'Keefe Clouds"
                    , id = "clouds"
                    , draw = Clouds.EffectView.draw
                    , mods =
                        [ ( Extremity, "funkitude", .extremity )
                        , ( Speed, "speed", .speed )
                        ]
                    , model = Clouds.Model.init flags
                    , tick = Clouds.Update.tick
                    , modConstructor = CloudMod
                    , applyModifier =
                        \m mod val ->
                            case mod of
                                Extremity ->
                                    { m | extremity = val }

                                Speed ->
                                    { m | speed = val }
                    }
            , WaveClockEffect <|
                Effects.build
                    { name = "Wave Clock Redux"
                    , id = "wave-clock"
                    , draw = WaveClock.EffectView.draw
                    , mods =
                        [ ( RadNoise, "radnoise", .modifiers >> .radNoise >> (\n -> n / 2) )
                        , ( AngNoise, "angnoise", .modifiers >> .angNoise >> (\n -> n / 2) )
                        , ( Radius, "rad", .modifiers >> .radius >> (\n -> n / 2) )
                        , ( Step, "step", .modifiers >> .step >> (\n -> n / 2) )
                        , ( Delay, "delay", .modifiers >> .delay )
                        ]
                    , model = WaveClock.Model.init flags
                    , tick = WaveClock.Update.tick
                    , modConstructor = WaveClockMod
                    , applyModifier = WaveClock.Update.modify
                    }
            , SutcliffeEffect <|
                Effects.build
                    { name = "Sutcliffe Pentagons"
                    , id = "sutcliffe"
                    , draw = Sutcliffe.EffectView.draw
                    , mods =
                        []
                    , model = Sutcliffe.Model.init flags
                    , tick = Sutcliffe.Update.tick
                    , modConstructor = SutcliffeMod
                    , applyModifier = Sutcliffe.Update.modify
                    }
            ]
      }
    , Cmd.none
    )
