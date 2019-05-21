module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Circle2d as Circle
import Clouds.Model exposing (..)
import Clouds.Update
import Interop
import List.Extra as List
import Messages exposing (..)
import Point2d as Point
import Random
import Random.Extra as Random
import Time exposing (posixToMillis)
import View


title =
    "O'Keefe Clouds"


main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = title
                , body = View.draw model
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
            ( Clouds.Update.tick time model, Cmd.none )

        ModifierChanged mod val ->
            ( case mod of
                Extremity ->
                    { model | extremity = val }
            , Cmd.none
            )


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ onAnimationFrame AnimationFrameTriggered
        , Interop.midiInput MidiInputReceived
        ]
