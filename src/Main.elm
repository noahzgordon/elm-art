module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Circle2d as Circle
import Clouds.Update
import Effects
import Interop
import List.Extra as List
import Messages exposing (..)
import Model exposing (MetaEffect(..), Model)
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
                        { title = "O'Keefe Clouds"
                        , body = View.draw eff
                        }

                    LightningEffect eff ->
                        { title = "Fork Lightning"
                        , body = View.draw eff
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


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ onAnimationFrame AnimationFrameTriggered
        , Interop.midiInput MidiInputReceived
        ]
