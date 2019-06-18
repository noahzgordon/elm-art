module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Circle2d as Circle
import Clouds.Update
import Effects
import Interop
import Json.Decode as Json
import List.Extra as List
import Messages exposing (..)
import Model exposing (Model)
import Point2d as Point
import Random
import Random.Extra as Random
import Time exposing (posixToMillis)
import View


main =
    Browser.document
        { init = Model.init
        , view =
            \model ->
                case model.currentEffect of
                    CloudEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    LightningEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    NoiseEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    NoiseOverTimeEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    Noise2dEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    WaveClockEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }

                    SutcliffeEffect eff ->
                        { title = Effects.name eff
                        , body = View.draw eff model.otherEffects
                        }
        , update = update
        , subscriptions = subscriptions
        }


type alias MidiMessage =
    { status : Int
    , dataOne : Int
    , dataTwo : Int
    }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        MidiInputReceived val ->
            case
                Json.decodeValue
                    (Json.map3 MidiMessage
                        (Json.field "status" Json.int)
                        (Json.field "dataOne" Json.int)
                        (Json.field "dataTwo" Json.int)
                    )
                    val
            of
                Ok { status, dataOne, dataTwo } ->
                    case status of
                        -- control change
                        176 ->
                            let
                                modify : Effect model modifier -> Effect model modifier
                                modify eff =
                                    case
                                        Effects.modifiers eff
                                            |> List.getAt (dataOne - 1)
                                    of
                                        Nothing ->
                                            eff

                                        Just ( modifier, _, _ ) ->
                                            Effects.applyModifier eff modifier (toFloat dataTwo / 127)
                            in
                            ( { model
                                | currentEffect =
                                    case model.currentEffect of
                                        CloudEffect eff ->
                                            CloudEffect (modify eff)

                                        LightningEffect eff ->
                                            LightningEffect (modify eff)

                                        NoiseEffect eff ->
                                            NoiseEffect (modify eff)

                                        NoiseOverTimeEffect eff ->
                                            NoiseOverTimeEffect (modify eff)

                                        Noise2dEffect eff ->
                                            Noise2dEffect (modify eff)

                                        WaveClockEffect eff ->
                                            WaveClockEffect (modify eff)

                                        SutcliffeEffect eff ->
                                            SutcliffeEffect (modify eff)
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AnimationFrameTriggered time ->
            ( { model
                | currentEffect =
                    case model.currentEffect of
                        CloudEffect eff ->
                            CloudEffect <|
                                Effects.tick eff time

                        LightningEffect eff ->
                            LightningEffect <|
                                Effects.tick eff time

                        NoiseEffect eff ->
                            NoiseEffect <|
                                Effects.tick eff time

                        NoiseOverTimeEffect eff ->
                            NoiseOverTimeEffect <|
                                Effects.tick eff time

                        Noise2dEffect eff ->
                            Noise2dEffect <|
                                Effects.tick eff time

                        WaveClockEffect eff ->
                            WaveClockEffect <|
                                Effects.tick eff time

                        SutcliffeEffect eff ->
                            SutcliffeEffect <|
                                Effects.tick eff time
              }
            , Cmd.none
            )

        ModifierChanged mod val ->
            ( case ( model.currentEffect, mod ) of
                ( CloudEffect eff, CloudMod mod_ ) ->
                    { model
                        | currentEffect =
                            CloudEffect <|
                                Effects.applyModifier eff mod_ val
                    }

                ( LightningEffect eff, LightningMod mod_ ) ->
                    { model
                        | currentEffect =
                            LightningEffect <|
                                Effects.applyModifier eff mod_ val
                    }

                ( WaveClockEffect eff, WaveClockMod mod_ ) ->
                    { model
                        | currentEffect =
                            WaveClockEffect <|
                                Effects.applyModifier eff mod_ val
                    }

                ( SutcliffeEffect eff, SutcliffeMod mod_ ) ->
                    { model
                        | currentEffect =
                            SutcliffeEffect <|
                                Effects.applyModifier eff mod_ val
                    }

                _ ->
                    model
            , Cmd.none
            )

        UserSelectedEffect eff ->
            ( { model
                | currentEffect = eff
                , otherEffects =
                    model.otherEffects
                        |> List.filter
                            (\otherEff ->
                                -- this should be easier!
                                case eff of
                                    CloudEffect _ ->
                                        case otherEff of
                                            CloudEffect _ ->
                                                False

                                            _ ->
                                                True

                                    LightningEffect _ ->
                                        case otherEff of
                                            LightningEffect _ ->
                                                False

                                            _ ->
                                                True

                                    NoiseEffect _ ->
                                        case otherEff of
                                            NoiseEffect _ ->
                                                False

                                            _ ->
                                                True

                                    NoiseOverTimeEffect _ ->
                                        case otherEff of
                                            NoiseOverTimeEffect _ ->
                                                False

                                            _ ->
                                                True

                                    Noise2dEffect _ ->
                                        case otherEff of
                                            Noise2dEffect _ ->
                                                False

                                            _ ->
                                                True

                                    WaveClockEffect _ ->
                                        case otherEff of
                                            WaveClockEffect _ ->
                                                False

                                            _ ->
                                                True

                                    SutcliffeEffect _ ->
                                        case otherEff of
                                            SutcliffeEffect _ ->
                                                False

                                            _ ->
                                                True
                            )
                        |> List.append [ model.currentEffect ]
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ onAnimationFrame AnimationFrameTriggered
        , Interop.midiInput MidiInputReceived
        ]
