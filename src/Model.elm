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
import Time exposing (Posix)


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
                    , draw = Noise.EffectView.draw
                    , mods = []
                    , model = Noise.Model.init flags
                    , tick = \t m -> { m | time = Time.posixToMillis t }
                    , modConstructor = NoiseMod
                    , applyModifier = \eff _ _ -> eff
                    }
      , otherEffects =
            [ LightningEffect <|
                Effects.build
                    { name = "Fork Lightning"
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
                        \effect mod val ->
                            case mod of
                                Fremulation ->
                                    Effects.updateModel effect
                                        (\m -> { m | fremulation = val })

                                Chaos ->
                                    Effects.updateModel effect
                                        (\m -> { m | chaos = val })

                                Dilation ->
                                    Effects.updateModel effect
                                        (\m -> { m | dilation = val })

                                Zoom ->
                                    Effects.updateModel effect
                                        (\m -> { m | zoom = val })
                    }
            , CloudEffect <|
                Effects.build
                    { name = "O'Keefe Clouds"
                    , draw = Clouds.EffectView.draw
                    , mods =
                        [ ( Extremity, "funkitude", .extremity )
                        , ( Speed, "speed", .speed )
                        ]
                    , model = Clouds.Model.init flags
                    , tick = Clouds.Update.tick
                    , modConstructor = CloudMod
                    , applyModifier =
                        \effect mod val ->
                            case mod of
                                Extremity ->
                                    Effects.updateModel effect
                                        (\m -> { m | extremity = val })

                                Speed ->
                                    Effects.updateModel effect
                                        (\m -> { m | speed = val })
                    }
            ]
      }
    , Cmd.none
    )
