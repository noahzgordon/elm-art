module WaveClock.Update exposing (Modifier(..), modify, tick)

import Color exposing (rgba)
import Perlin exposing (noise)
import Time exposing (Posix)
import WaveClock.Model exposing (Model)


type Modifier
    = RadNoise
    | AngNoise
    | Radius
    | Step
    | Delay


tick : Posix -> Model -> Model
tick time model =
    let
        timeInt =
            Time.posixToMillis time
    in
    if timeInt - (round <| model.modifiers.delay * 1000) < model.lastTick then
        model

    else
        let
            imageWidth =
                model.window.width - 200

            radNoise =
                model.angNoise + 0.005

            radius =
                (noise ( radNoise * model.modifiers.radNoise, 0, 0 ) model.seed * 550 * model.modifiers.radius) + 1

            angNoise =
                model.angNoise + 0.005

            baseAngleStep =
                6 * model.modifiers.step

            baseAngle =
                model.angle
                    + (noise ( model.angNoise * model.modifiers.angNoise, 0, 0 ) model.seed * baseAngleStep)
                    - 3

            angle =
                if baseAngle > 360 then
                    baseAngle - 360

                else if baseAngle < 0 then
                    baseAngle + 360

                else
                    baseAngle

            ( xNoise, yNoise ) =
                ( model.xNoise + 0.01
                , model.yNoise + 0.01
                )

            ( centerX, centerY ) =
                ( imageWidth / 2 + (noise ( xNoise, 0, 0 ) model.seed * 100) - 50
                , model.window.height / 2 + (noise ( yNoise, 0, 0 ) model.seed * 100) - 50
                )

            rad =
                radians angle

            oppRad =
                rad + pi

            colorVal =
                model.colorVal + model.colorChange
        in
        { model
            | angle = angle
            , angNoise = angNoise
            , radNoise = radNoise
            , xNoise = xNoise
            , yNoise = yNoise
            , colorVal = colorVal
            , colorChange =
                if colorVal > 254 then
                    -1

                else if colorVal < 0 then
                    1

                else
                    model.colorChange
            , lines =
                model.lines
                    ++ [ { x1 = centerX + (radius * cos rad)
                         , y1 = centerY + (radius * sin rad)
                         , x2 = centerX + (radius * cos oppRad)
                         , y2 = centerY + (radius * sin oppRad)
                         , color = rgba (colorVal / 255) (colorVal / 255) (colorVal / 255) (60 / 255)
                         }
                       ]
            , lastTick = timeInt
        }


modify : Model -> Modifier -> Float -> Model
modify model mod val =
    let
        modifiers =
            model.modifiers
    in
    case mod of
        RadNoise ->
            { model | modifiers = { modifiers | radNoise = Debug.log "Val" <| val * 2 } }

        AngNoise ->
            { model | modifiers = { modifiers | angNoise = val * 2 } }

        Radius ->
            { model | modifiers = { modifiers | radius = val * 2 } }

        Step ->
            { model | modifiers = { modifiers | step = val * 2 } }

        Delay ->
            { model | modifiers = { modifiers | delay = val } }
