module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Circle2d as Circle
import Clouds.Update
import Effects
import Interop
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
        , update = update
        , subscriptions = subscriptions
        }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        MidiInputReceived val ->
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

                                            LightningEffect _ ->
                                                True

                                    LightningEffect _ ->
                                        case otherEff of
                                            CloudEffect _ ->
                                                True

                                            LightningEffect _ ->
                                                False
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
